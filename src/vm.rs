use std::fmt;

type Result<T> = std::result::Result<T, String>;
#[derive(Debug)]
struct Stack<T>(Vec<T>);

impl<T> Stack<T> {
    fn new() -> Self {
        Self(Vec::new())
    }

    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn pop(&mut self) -> Option<T> {
        self.0.pop()
    }

    fn push(&mut self, value: T) {
        self.0.push(value)
    }
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    Boolean(bool),
    Integer(i32),
    NativeFn(fn(Vec<Value>) -> Result<Value>),
    Null,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean(false) => write!(f, "#f"),
            Self::Boolean(true) => write!(f, "#t"),
            Self::Integer(value) => write!(f, "{}", value),
            Self::NativeFn(_) => write!(f, "#<native function>"),
            Self::Null => write!(f, "'()"),
        }
    }
}

impl Value {
    const FALSE: Self = Self::Boolean(false);
    const NULL: Self = Self::Null;
    const TRUE: Self = Self::Boolean(true);

    fn is_falsy(&self) -> bool {
        match self {
            Value::Boolean(false) => true,
            _ => false,
        }
    }

    fn is_null(&self) -> bool {
        match self {
            Value::Null => true,
            _ => false,
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Value::Boolean(false) => false,
            _ => true,
        }
    }
}

impl From<bool> for Value {
    fn from(boolean: bool) -> Self {
        if boolean {
            Self::TRUE
        } else {
            Self::FALSE
        }
    }
}

#[derive(Debug)]
struct StackFrame {
    base_ptr: usize,
    return_address: usize,
}

impl StackFrame {
    fn new(base_ptr: usize, return_address: usize) -> Self {
        Self {
            base_ptr,
            return_address,
        }
    }
}

enum Bytecode {
    Call(usize),
    Boolean(bool),
    Div,
    Eq,
    Gt,
    If(usize),
    Integer(i32),
    Lt,
    Mul,
    Neg,
    Null,
    Rem,
    Return,
    Sub,
}

#[derive(Debug)]
struct Vm {
    instruction_ptr: usize,
    call_stack: Stack<StackFrame>,
    operand_stack: Stack<Value>,
}

impl Vm {
    fn new() -> Self {
        Self {
            instruction_ptr: 0,
            call_stack: Stack::new(),
            operand_stack: Stack::new(),
        }
    }

    fn execute(&mut self, bytecodes: Vec<Bytecode>) -> Result<()> {
        loop {
            dbg!(&self);
            match bytecodes[self.instruction_ptr] {
                Bytecode::Boolean(immediate) => self.operand_stack.push(immediate.into()),
                Bytecode::Call(addr) => {
                    let frame = StackFrame::new(self.operand_stack.len(), self.instruction_ptr + 1);
                    self.call_stack.push(frame);
                    self.instruction_ptr = addr;
                    continue;
                }
                Bytecode::Div => {
                    let lhs = match self.operand_stack.pop() {
                        Some(Value::Integer(lhs)) => lhs,
                        Some(value) => return Err(format!("Div: invalid first operand on the stack: expected integer, found {}", value)),
                        None => return Err("Div: invalid first operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    let rhs = match self.operand_stack.pop() {
                        Some(Value::Integer(rhs)) => rhs,
                        Some(value) => return Err(format!("Div: invalid second operand on the stack: expected integer, found {}", value)),
                        None => return Err("Div: invalid second operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    if rhs != 0 {
                        self.operand_stack.push(Value::Integer(lhs / rhs));
                    } else {
                        return Err("Div: division by zero".to_owned());
                    }
                }
                Bytecode::Eq => {
                    let lhs = match self.operand_stack.pop() {
                        Some(Value::Integer(lhs)) => lhs,
                        Some(value) => return Err(format!("Eq: invalid first operand on the stack: expected integer, found {}", value)),
                        None => return Err("Eq: invalid first operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    let rhs = match self.operand_stack.pop() {
                        Some(Value::Integer(rhs)) => rhs,
                        Some(value) => return Err(format!("Eq: invalid second operand on the stack: expected integer, found {}", value)),
                        None => return Err("Eq: invalid second operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    self.operand_stack.push(Value::Boolean(lhs == rhs));
                }
                Bytecode::Gt => {
                    let lhs = match self.operand_stack.pop() {
                        Some(Value::Integer(lhs)) => lhs,
                        Some(value) => return Err(format!("Gt: invalid first operand on the stack: expected integer, found {}", value)),
                        None => return Err("Gt: invalid first operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    let rhs = match self.operand_stack.pop() {
                        Some(Value::Integer(rhs)) => rhs,
                        Some(value) => return Err(format!("Gt: invalid second operand on the stack: expected integer, found {}", value)),
                        None => return Err("Gt: invalid second operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    self.operand_stack.push(Value::Boolean(lhs > rhs));
                }
                Bytecode::If(offset) => match self.operand_stack.pop() {
                    Some(value) => {
                        if value.is_falsy() {
                            self.instruction_ptr += offset;
                            continue;
                        }
                    }
                    None => return Err("If: expected 1 operand on the stack, found 0".to_owned()),
                },
                Bytecode::Integer(immediate) => self.operand_stack.push(Value::Integer(immediate)),
                Bytecode::Lt => {
                    let lhs = match self.operand_stack.pop() {
                        Some(Value::Integer(lhs)) => lhs,
                        Some(value) => return Err(format!("Lt: invalid first operand on the stack: expected integer, found {}", value)),
                        None => return Err("Lt: invalid first operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    let rhs = match self.operand_stack.pop() {
                        Some(Value::Integer(rhs)) => rhs,
                        Some(value) => return Err(format!("Lt: invalid second operand on the stack: expected integer, found {}", value)),
                        None => return Err("Lt: invalid second operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    self.operand_stack.push(Value::Boolean(lhs < rhs));
                }
                Bytecode::Mul => {
                    let lhs = match self.operand_stack.pop() {
                        Some(Value::Integer(lhs)) => lhs,
                        Some(value) => return Err(format!("Mul: invalid first operand on the stack: expected integer, found {}", value)),
                        None => return Err("Mul: invalid first operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    let rhs = match self.operand_stack.pop() {
                        Some(Value::Integer(rhs)) => rhs,
                        Some(value) => return Err(format!("Mul: invalid second operand on the stack: expected integer, found {}", value)),
                        None => return Err("Mul: invalid second operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    self.operand_stack.push(Value::Integer(lhs * rhs));
                }
                Bytecode::Neg => match self.operand_stack.pop() {
                    Some(Value::Integer(n)) => self.operand_stack.push(Value::Integer(-n)),
                    Some(value) => {
                        return Err(format!(
                            "Neg: invalid operand on the stack: expected integer, found {}",
                            value
                        ))
                    }
                    None => return Err(
                        "Neg: invalid operand on the stack: expected integer, found empty stack"
                            .to_owned(),
                    ),
                },
                Bytecode::Null => self.operand_stack.push(Value::Null),
                Bytecode::Rem => {
                    let lhs = match self.operand_stack.pop() {
                        Some(Value::Integer(lhs)) => lhs,
                        Some(value) => return Err(format!("Mod: invalid first operand on the stack: expected integer, found {}", value)),
                        None => return Err("Mod: invalid first operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    let rhs = match self.operand_stack.pop() {
                        Some(Value::Integer(rhs)) => rhs,
                        Some(value) => return Err(format!("Mod: invalid second operand on the stack: expected integer, found {}", value)),
                        None => return Err("Mod: invalid second operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    if rhs != 0 {
                        self.operand_stack.push(Value::Integer(lhs % rhs));
                    } else {
                        return Err("Rem: division by zero".to_owned());
                    }
                }
                Bytecode::Return => match self.call_stack.pop() {
                    Some(frame) => {
                        self.instruction_ptr = frame.return_address;
                        continue;
                    }
                    None => return Ok(()),
                },
                Bytecode::Sub => {
                    let lhs = match self.operand_stack.pop() {
                        Some(Value::Integer(lhs)) => lhs,
                        Some(value) => return Err(format!("Sub: invalid first operand on the stack: expected integer, found {}", value)),
                        None => return Err("Sub: invalid first operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    let rhs = match self.operand_stack.pop() {
                        Some(Value::Integer(rhs)) => rhs,
                        Some(value) => return Err(format!("Sub: invalid second operand on the stack: expected integer, found {}", value)),
                        None => return Err("Sub: invalid second operand on the stack: expected integer, found empty stack".to_owned()),
                    };
                    self.operand_stack.push(Value::Integer(lhs - rhs));
                }
            }
            self.instruction_ptr += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_return() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 0);
        assert!(vm.call_stack.is_empty());
        assert!(vm.operand_stack.is_empty());
    }

    #[test]
    fn test_integer() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(0),
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 1);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(0)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(1),
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 1);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(1)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(-1),
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 1);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(-1)));
        assert!(vm.operand_stack.is_empty());
    }

    #[test]
    fn test_boolean() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Boolean(false),
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 1);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Boolean(false)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Boolean(true),
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 1);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Boolean(true)));
        assert!(vm.operand_stack.is_empty());
    }

    #[test]
    fn test_null() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Null,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 1);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::NULL));
        assert!(vm.operand_stack.is_empty());
    }

    #[test]
    fn test_call() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Call(2),
            Bytecode::Return,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 1);
        assert!(vm.call_stack.is_empty());
        assert!(vm.operand_stack.is_empty());
    }

    #[test]
    fn test_div() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(2),
            Bytecode::Integer(4),
            Bytecode::Div,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(2)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(0),
            Bytecode::Integer(1),
            Bytecode::Div,
            Bytecode::Return,
        ]).unwrap_err();

        assert_eq!(vm.instruction_ptr, 2);
        assert!(vm.call_stack.is_empty());
        assert!(vm.operand_stack.is_empty());
    }

    #[test]
    fn test_eq() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(1),
            Bytecode::Integer(1),
            Bytecode::Eq,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Boolean(true)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(0),
            Bytecode::Integer(1),
            Bytecode::Eq,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Boolean(false)));
        assert!(vm.operand_stack.is_empty());
    }

    #[test]
    fn test_gt() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(0),
            Bytecode::Integer(1),
            Bytecode::Gt,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Boolean(true)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(1),
            Bytecode::Integer(1),
            Bytecode::Gt,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Boolean(false)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(1),
            Bytecode::Integer(0),
            Bytecode::Gt,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Boolean(false)));
        assert!(vm.operand_stack.is_empty());
    }

    #[test]
    fn test_if() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Boolean(true),
            Bytecode::If(3),
            Bytecode::Boolean(true),
            Bytecode::Return,
            Bytecode::Boolean(false),
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Boolean(true)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Boolean(false),
            Bytecode::If(3),
            Bytecode::Boolean(true),
            Bytecode::Return,
            Bytecode::Boolean(false),
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 5);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Boolean(false)));
        assert!(vm.operand_stack.is_empty());
    }

    #[test]
    fn test_lt() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(0),
            Bytecode::Integer(1),
            Bytecode::Lt,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Boolean(false)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(1),
            Bytecode::Integer(1),
            Bytecode::Lt,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Boolean(false)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(1),
            Bytecode::Integer(0),
            Bytecode::Lt,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Boolean(true)));
        assert!(vm.operand_stack.is_empty());
    }

    #[test]
    fn test_mul() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(2),
            Bytecode::Integer(4),
            Bytecode::Mul,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(8)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(0),
            Bytecode::Integer(1),
            Bytecode::Mul,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(0)));
        assert!(vm.operand_stack.is_empty());
    }

    #[test]
    fn test_neg() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(0),
            Bytecode::Neg,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 2);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(0)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(1),
            Bytecode::Neg,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 2);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(-1)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(-1),
            Bytecode::Neg,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 2);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(1)));
        assert!(vm.operand_stack.is_empty());
    }

    #[test]
    fn test_rem() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(3),
            Bytecode::Integer(10),
            Bytecode::Rem,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(1)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(-3),
            Bytecode::Integer(10),
            Bytecode::Rem,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(1)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(3),
            Bytecode::Integer(-10),
            Bytecode::Rem,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(-1)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(-3),
            Bytecode::Integer(-10),
            Bytecode::Rem,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(-1)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(0),
            Bytecode::Integer(1),
            Bytecode::Rem,
            Bytecode::Return,
        ]).unwrap_err();

        assert_eq!(vm.instruction_ptr, 2);
        assert!(vm.call_stack.is_empty());
        assert!(vm.operand_stack.is_empty());
    }

    #[test]
    fn test_sub() {
        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(2),
            Bytecode::Integer(4),
            Bytecode::Sub,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(2)));
        assert!(vm.operand_stack.is_empty());

        let mut vm = Vm::new();
        #[rustfmt::skip]
        vm.execute(vec![
            Bytecode::Integer(0),
            Bytecode::Integer(1),
            Bytecode::Sub,
            Bytecode::Return,
        ]).unwrap();

        assert_eq!(vm.instruction_ptr, 3);
        assert!(vm.call_stack.is_empty());
        assert_eq!(vm.operand_stack.pop(), Some(Value::Integer(1)));
        assert!(vm.operand_stack.is_empty());
    }
}
