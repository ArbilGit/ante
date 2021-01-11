//! nameresolution/builtin.rs - Helpers for importing the prelude
//! and defining some builtin symbols such as the `string` type (which
//! is builtin because otherwise string literals may be used before the
//! actual string type is defined) and the `builtin` function which is
//! used by codegen to stand in place of primitive operations like adding
//! integers together.
use crate::cache::{ ModuleCache, DefinitionInfoId };
use crate::error::location::Location;
use crate::lexer::token::IntegerKind;
use crate::nameresolution::{ NameResolver, declare_module, define_module };
use crate::types::{ Type, PrimitiveType, TypeInfoBody, Field, STRING_TYPE, LetBindingLevel };

use std::path::PathBuf;

/// The DefinitionInfoId of the `builtin` symbol is defined to be
/// the first id. This invariant needs to be maintained by the
/// `define_builtins` function here being called before any other
/// symbol is defined. This is asserted to be the case within that function.
pub const BUILTIN_ID: DefinitionInfoId = DefinitionInfoId(0);

/// Defines the builtin symbols:
/// - `type string = c_string: ref char, len: usz`
/// - `builtin : string -> a` used by the codegen pass to implement
///   codegen of builtin operations such as adding integers.
///
/// This function needs to be called before any other DefinitionInfoId is
/// created, otherwise the `builtin` symbol will have the wrong id. If this
/// happens, this function will assert at runtime.
pub fn define_builtins<'a>(cache: &mut ModuleCache<'a>) {
    let string_type = define_string(cache);

    // Define builtin : forall a. string -> a imported only into the prelude to define
    // builtin operations by name. The specific string arguments are matched on in src/llvm/builtin.rs
    let id = cache.push_definition("builtin", false, Location::builtin());
    assert!(id == BUILTIN_ID);

    let a = cache.next_type_variable_id(LetBindingLevel(1));
    let info = &mut cache.definition_infos[id.0];

    let builtin_fn_type = Type::Function(vec![string_type], Box::new(Type::TypeVariable(a)), false);
    let builtin_type= Type::ForAll(vec![a], Box::new(builtin_fn_type));
    info.typ = Some(builtin_type);
}

/// The prelude is currently stored (along with the rest of the stdlib) in the
/// user's config directory since it is a cross-platform concept that doesn't
/// require administrator priviledges.
pub fn prelude_path() -> PathBuf {
    dirs::config_dir().unwrap().join("ante/stdlib/prelude.an")
}

pub fn import_prelude<'a>(resolver: &mut NameResolver, cache: &mut ModuleCache<'a>) {
    if resolver.filepath == prelude_path() {
        // If we're in the prelude include the built-in symbol "builtin" to define primitives
        resolver.current_scope().definitions.insert("builtin".into(), BUILTIN_ID);
    } else {
        // Otherwise, import the prelude itself
        let prelude_dir = prelude_path();
        cache.prelude_path = prelude_dir.clone();

        if let Some(id) = declare_module(&prelude_dir, cache, Location::builtin()) {
            let exports = define_module(id, cache, Location::builtin()).unwrap();
            resolver.current_scope().import(exports, cache, Location::builtin());
        }
    }

    // Manually insert the Int trait as if it were defined in the prelude
    resolver.current_scope().traits.insert("Int".into(), cache.int_trait);
}

/// Defining the 'string' type is a bit different than most other builtins. Since 'string' has
/// its own dedicated keyword it need not be imported into scope like each impl of + or - does.
fn define_string<'a>(cache: &mut ModuleCache<'a>) -> Type {
    let location = Location::builtin();

    let ref_type = Type::Primitive(PrimitiveType::ReferenceType);
    let char_type = Type::Primitive(PrimitiveType::CharType);
    let c_string_type = Type::TypeApplication(Box::new(ref_type), vec![char_type]);

    let length_type = Type::Primitive(PrimitiveType::IntegerType(IntegerKind::Usz));

    let string = cache.push_type_info("string".into(), vec![], location);
    assert!(string == STRING_TYPE);

    cache.type_infos[string.0].body = TypeInfoBody::Struct(vec![
        Field { name: "c_string".into(), field_type: c_string_type, location },
        Field { name: "length".into(),   field_type: length_type,   location },
    ]);

    Type::UserDefinedType(STRING_TYPE)
}
