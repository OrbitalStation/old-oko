use crate::*;

pub fn bake_types() {
    for builtin in BUILTIN_TYPES {
        Type::meet_new_raw_scalar(builtin.name.to_string(), None);
    }

    let types = core::mem::replace(match Type::type_list() {
        TypeList::Raw(raw) => raw,
        _ => unimplemented!()
    }, vec![]);

    let types = types.into_iter().map(|ty| match ty {
        RawType::Backed(typedef) => BakedType::ordinary(typedef),
        RawType::Stub(stub) => if let Some((idx, _)) = BUILTIN_TYPES.iter().enumerate().find(|(_, x)| x.name == stub) {
            BakedType::builtin(idx)
        } else {
            panic!("unknown type: {stub}")
        }
    }).collect();

    *Type::type_list() = TypeList::Baked(types)
}
