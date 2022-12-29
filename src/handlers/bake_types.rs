use std::collections::VecDeque;
use crate::*;

pub fn bake_types() {
    for builtin in BUILTIN_TYPES {
        Type::meet_new_raw_scalar(None, builtin.name.to_string(), None);
    }

    let types = core::mem::replace(match Type::type_list() {
        TypeList::Raw(raw) => raw,
        _ => unimplemented!()
    }, vec![]);

    let mut baked_locs = VecDeque::new();
    for i in &types {
        if let RawType::Seq {
            start_idx,
            continuation
        } = i {
            let mut cur = &types[*start_idx];
            let mut loc = TypeKindScalarLocation::Global { index: *start_idx };
            for part in continuation {
                match cur {
                    RawType::Backed(backed) => match &backed.subtypes {
                        TypeList::Raw(raw) => match raw.iter().enumerate().find(|(_, x)| x.name() == *part) {
                            Some((index, x)) => {
                                cur = x;
                                loc = TypeKindScalarLocation::AssociatedItem {
                                    index,
                                    mother: Box::new(loc),
                                }
                            },
                            None => panic!("type `{}` does not contain an associated type named `{part}`", cur.name())
                        },
                        _ => unreachable!()
                    },
                    _ => panic!("type `{}` does not contain any associated types", types[*start_idx].name())
                }
            }
            baked_locs.push_back(loc)
        }
    }

    let types = types.into_iter().map(|x| handle_raw_type(x, String::new(), &mut baked_locs)).collect();

    *Type::type_list() = TypeList::Baked(types);

    for ty in Type::baked() {
        if let BakedTypeKind::Alias(loc) = &mut ty.kind {
            ty.llvm_type = loc.baked().unwrap().llvm_type
        }
    }
}

fn handle_raw_type(ty: RawType, prefix: String, baked_locs: &mut VecDeque <TypeKindScalarLocation>) -> BakedType {
    match ty {
        RawType::Backed(mut typedef) => {
            let raw = core::mem::replace(match &mut typedef.subtypes {
                TypeList::Raw(raw) => raw,
                _ => unreachable!()
            }, vec![]);
            let subtypes_prefix = if prefix.is_empty() {
                typedef.name.clone()
            } else {
                format!("{prefix}.{}", typedef.name)
            };
            let baked = raw.into_iter().map(|x| handle_raw_type(x, subtypes_prefix.clone(), &mut VecDeque::new())).collect();
            typedef.subtypes = TypeList::Baked(baked);
            BakedType::ordinary(typedef, subtypes_prefix)
        },
        RawType::Stub(stub) => if let Some((idx, _)) = BUILTIN_TYPES.iter().enumerate().find(|(_, x)| x.name == stub) {
            BakedType::builtin(idx)
        } else {
            panic!("unknown type: {stub}")
        },
        RawType::Seq { .. } => BakedType::alias(baked_locs.pop_front().unwrap())
    }
}
