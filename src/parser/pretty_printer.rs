//! Defines a simple pretty printer to print the Ast to stdout.
//! Used for the golden tests testing parsing to ensure there
//! are no parsing regressions.
use crate::cache::ModuleCache;
use crate::parser::ast::{ self, Ast };
use crate::util::reinterpret_from_bits;
use std::fmt::{ self, Display, Formatter };
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;

static INDENT_LEVEL: AtomicUsize = AtomicUsize::new(0);

struct NodePrinter<'local, 'cache> {
    node: &'local Ast<'cache>,
    cache: &'local ModuleCache<'cache>,
}

pub trait MyDisplay {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result;
}

pub fn print<'c>(node: &Ast<'c>, cache: &ModuleCache<'c>) {
    let printer = NodePrinter { node, cache };
    println!("{}", printer);
}

fn to_string<'c>(node: &Ast<'c>, cache: &ModuleCache<'c>) -> String {
    let printer = NodePrinter { node, cache };
    printer.to_string()
}

impl<'l, 'c> Display for NodePrinter<'l, 'c> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        MyDisplay::fmt(self.node, f, self.cache)
    }
}

impl<'a> MyDisplay for Ast<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        dispatch_on_expr!(self, MyDisplay::fmt, f, cache)
    }
}

impl MyDisplay for ast::AstId {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        MyDisplay::fmt(cache.get_node(*self), f, cache)
    }
}

impl<'a> MyDisplay for ast::Literal<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, _cache: &ModuleCache<'_>) -> fmt::Result {
        use ast::LiteralKind::*;
        match &self.kind {
            Integer(x, _) => write!(f, "{}", x),
            Float(x) => write!(f, "{}", reinterpret_from_bits(*x)),
            String(s) => write!(f, "\"{}\"", s),
            Char(c) => write!(f, "'{}'", c),
            Bool(b) => write!(f, "{}", if *b { "true" } else { "false" }),
            Unit => write!(f, "()"),
        }
    }
}

impl<'a> MyDisplay for ast::Variable<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, _cache: &ModuleCache<'_>) -> fmt::Result {
        use ast::VariableKind::*;
        match &self.kind {
            Identifier(name) => write!(f, "{}", name),
            Operator(token) => write!(f, "{}", token),
            TypeConstructor(name) => write!(f, "{}", name),
        }
    }
}

impl<'a> MyDisplay for ast::Lambda<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(\\")?;
        for arg in self.args.iter() {
            let node = cache.get_node(*arg);
            write!(f, " ")?;
            MyDisplay::fmt(node, f, cache)?;
        }
        if let Some(typ) = &self.return_type {
            write!(f, " -> ")?;
            MyDisplay::fmt(typ, f, cache)?;
        }
        write!(f, " . ")?;
        MyDisplay::fmt(cache.get_node(self.body), f, cache)?;
        write!(f, ")")
    }
}

pub fn join_with<'c, T: MyDisplay>(vec: &[T], delimiter: &str, f: &mut Formatter<'_>, cache: &ModuleCache<'c>) {
    let len = vec.len();
    if len == 0 {
        return;
    }

    for i in 0 .. len - 1 {
        MyDisplay::fmt(&vec[i], f, cache).unwrap();
        write!(f, "{}", delimiter).unwrap();
    }

    MyDisplay::fmt(&vec[len - 1], f, cache).unwrap();
}

impl<'a> MyDisplay for ast::FunctionCall<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(")?;
        MyDisplay::fmt(&self.function, f, cache)?;
        write!(f, " ")?;
        join_with(&self.args, " ", f, cache);
        write!(f, ")")
    }
}

impl<'a> MyDisplay for ast::Definition<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(")?;
        MyDisplay::fmt(&self.pattern, f, cache)?;
        write!(f, " = ")?;
        MyDisplay::fmt(&self.expr, f, cache)?;
        write!(f, ")")
    }
}

impl<'a> MyDisplay for ast::If<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(if ")?;
        MyDisplay::fmt(&self.condition, f, cache)?;
        write!(f, " then ")?;
        MyDisplay::fmt(&self.then, f, cache)?;

        if let Some(ref otherwise) = self.otherwise {
            write!(f, " else ")?;
            MyDisplay::fmt(otherwise, f, cache)?;
        }
        write!(f, ")")
    }
}

impl<'a> MyDisplay for ast::Match<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(match ")?;
        MyDisplay::fmt(&self.expression, f, cache)?;

        for (pattern, branch) in self.branches.iter() {
            write!(f, "(")?;
            MyDisplay::fmt(pattern, f, cache)?;
            write!(f, " ")?;
            MyDisplay::fmt(branch, f, cache)?;
            write!(f, ")")?;
        }
        write!(f, ")")
    }
}

impl<'a> MyDisplay for ast::Type<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        use ast::Type::*;
        match self {
            IntegerType(kind, _) => write!(f, "{}", kind),
            FloatType(_) => write!(f, "float"),
            CharType(_) => write!(f, "char"),
            StringType(_) => write!(f, "string"),
            BooleanType(_) => write!(f, "bool"),
            UnitType(_) => write!(f, "unit"),
            ReferenceType(_) => write!(f, "ref"),
            TypeVariable(name, _) => write!(f, "{}", name),
            UserDefinedType(name, _) => write!(f, "{}", name),
            FunctionType(params, return_type, varargs, _) => {
                write!(f, "(")?;
                join_with(params, " ", f, cache);
                write!(f, " {}-> ", if *varargs { "... " } else { "" })?;
                MyDisplay::fmt(return_type.as_ref(), f, cache)?;
                write!(f, ")")
            },
            TypeApplication(constructor, args, _) => {
                write!(f, "(")?;
                MyDisplay::fmt(constructor.as_ref(), f, cache)?;
                write!(f, " ")?;
                join_with(args, " ", f, cache);
                write!(f, ")")
            },
            PairType(first, rest, _) => {
                write!(f, "(")?;
                MyDisplay::fmt(first.as_ref(), f, cache)?;
                write!(f, ", ")?;
                MyDisplay::fmt(rest.as_ref(), f, cache)?;
                write!(f, ")")
            }
        }
    }
}

impl<'a> MyDisplay for ast::TypeDefinitionBody<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        use ast::TypeDefinitionBody::*;
        match self {
            UnionOf(types) => {
                for (name, variant_fields, _) in types {
                    write!(f, "| {} ", name)?;
                    join_with(variant_fields, " ", f, cache);
                }
                Ok(())
            },
            StructOf(types) => {
                for t in types {
                    MyDisplay::fmt(&t.1, f, cache)?;
                    write!(f, ", ")?;
                }
                Ok(())
            },
            AliasOf(alias) => {
                MyDisplay::fmt(alias, f, cache)
            }
        }
    }
}

impl<'a> MyDisplay for ast::TypeDefinition<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(type {} {} = ", self.name, crate::util::join_with(&self.args, " "))?;
        MyDisplay::fmt(&self.definition, f, cache)?;
        write!(f, ")")
    }
}

impl<'a> MyDisplay for ast::TypeAnnotation<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(: ")?;
        MyDisplay::fmt(&self.lhs, f, cache)?;
        write!(f, " ")?;
        MyDisplay::fmt(&self.rhs, f, cache)?;
        write!(f, ")")
    }
}

impl<'a> MyDisplay for ast::Import<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, _cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(import {})", crate::util::join_with(&self.path, "."))
    }
}

impl<'a> MyDisplay for ast::TraitDefinition<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(trait {} {} ", self.name, crate::util::join_with(&self.args, " "))?;
        if !self.fundeps.is_empty() {
            write!(f, "-> {} ", crate::util::join_with(&self.fundeps, " "))?;
        }
        write!(f, "with\n    ")?;
        join_with(&self.declarations, "\n    ", f, cache);
        write!(f, "\n)")
    }
}

impl<'a> MyDisplay for ast::TraitImpl<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(impl {} ", self.trait_name)?;
        join_with(&self.trait_args, " ", f, cache);

        if !self.given.is_empty() {
            write!(f, " given ")?;
            join_with(&self.given, " ", f, cache);
        }

        write!(f, " with\n    ")?;
        join_with(&self.definitions, "\n    ", f, cache);
        write!(f, "\n)")
    }
}

impl<'a> MyDisplay for ast::Trait<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "({} ", self.name)?;
        join_with(&self.args, " ", f, cache);
        write!(f, ")")
    }
}

impl<'a> MyDisplay for ast::Return<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(return ")?;
        MyDisplay::fmt(&self.expression, f, cache)?;
        write!(f, ")")
    }
}

impl<'a> MyDisplay for ast::Sequence<'a> {
    /// Whenever printing out a Sequence, pretty-print the indented
    /// block as well so that larger programs are easier to read.
    ///
    /// To do this, each Sequence prepends 4 spaces to each line of
    /// the string form of its statements unless this is the top-level
    /// Sequence, in which case we don't want any spaces before the
    /// top-level definitions.
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        let mut statements = String::new();
        let indent_level = INDENT_LEVEL.fetch_add(1, Ordering::SeqCst);

        for (i, statement) in self.statements.iter().enumerate() {
            let statement = to_string(cache.get_node(*statement), cache);

            for line in statement.lines() {
                statements += "\n";
                if indent_level != 0 {
                    statements += "    ";
                }
                statements += line;
            }
            
            if i != self.statements.len() - 1 {
                statements += ";"
            }
        }

        INDENT_LEVEL.fetch_sub(1, Ordering::SeqCst);
        statements += "\n";
        write!(f, "{}", statements)?;
        Ok(())
    }
}

impl<'a> MyDisplay for ast::Extern<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(extern\n    ")?;
        join_with(&self.declarations, "\n    ", f, cache);
        write!(f, ")")
    }
}

impl<'a> MyDisplay for ast::MemberAccess<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(")?;
        MyDisplay::fmt(&self.lhs, f, cache)?;
        write!(f, ".{})", self.field)
    }
}

impl<'a> MyDisplay for ast::Assignment<'a> {
    fn fmt(&self, f: &mut Formatter<'_>, cache: &ModuleCache<'_>) -> fmt::Result {
        write!(f, "(")?;
        MyDisplay::fmt(&self.lhs, f, cache)?;
        write!(f, " := ")?;
        MyDisplay::fmt(&self.rhs, f, cache)?;
        write!(f, ")")
    }
}
