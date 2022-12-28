use crate::*;

pub fn bake_types() {
    for builtin in BUILTIN_TYPES {
        Type::meet_new_raw_scalar(None, builtin.name.to_string(), None);
    }

    let types = core::mem::replace(match Type::type_list() {
        TypeList::Raw(raw) => raw,
        _ => unimplemented!()
    }, vec![]);

    let types = types.into_iter().map(|x| handle_raw_type(x, String::new())).collect();

    *Type::type_list() = TypeList::Baked(types)
}

fn handle_raw_type(ty: RawType, prefix: String) -> BakedType {
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
            let baked = raw.into_iter().map(|x| handle_raw_type(x, subtypes_prefix.clone())).collect();
            typedef.subtypes = TypeList::Baked(baked);
            BakedType::ordinary(typedef, subtypes_prefix)
        },
        RawType::Stub(stub) => if let Some((idx, _)) = BUILTIN_TYPES.iter().enumerate().find(|(_, x)| x.name == stub) {
            BakedType::builtin(idx)
        } else {
            panic!("unknown type: {stub}")
        }
    }
}
