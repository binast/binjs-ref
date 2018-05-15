use ::{ TokenWriter, TokenWriterError };

use std::rc::Rc;
use std::io::Write;

use xml_rs;

#[derive(Debug)]
pub enum SubTree {
    String(Option<String>),
    Bool(Option<bool>),
    Float(Option<f64>),
    List(Vec<Rc<SubTree>>),
    Node {
        name: String,
        children: Vec<(String, Rc<SubTree>)>
    },
}

impl SubTree {
    fn write<W: Write>(&self, out: &mut xml_rs::writer::EventWriter<W>) -> xml_rs::writer::Result<()> {
        use xml_rs::writer::*;
        use self::SubTree::*;
        match *self {
            String(Some(ref s)) => { out.write(XmlEvent::characters(s.as_str()))?; }
            String(None) => {},
            Bool(Some(true)) => { out.write(XmlEvent::characters("true"))?; }
            Bool(Some(false)) => { out.write(XmlEvent::characters("false"))?; }
            Bool(None) => {},
            Float(Some(ref x)) => { out.write(XmlEvent::characters(&format!("{}", x)))?; }
            Float(None) => {},
            List(ref children) => {
                for c in children {
                    out.write(XmlEvent::start_element("_item"))?;
                    c.write(out)?;
                    out.write(XmlEvent::end_element())?;
                }
            }
            Node { ref name, ref children } => {
                let null = "_null";
                let name = if name.len() == 0 {
                    null
                } else {
                    name.as_str()
                };
                out.write(XmlEvent::start_element(name))?;
                for (ref name, ref c) in children {
                    out.write(XmlEvent::start_element(name.as_str()))?;
                    c.write(out)?;
                    out.write(XmlEvent::end_element())?;
                }
                out.write(XmlEvent::end_element())?;
            }
        }
        Ok(())
    }
}

pub struct Encoder {
    root: Rc<SubTree>,
}
impl Encoder {
    pub fn new() -> Self {
        Self {
            root: Rc::new(SubTree::Bool(None))
        }
    }

    fn register(&mut self, tree: SubTree) -> Result<Rc<SubTree>, TokenWriterError> {
        let result = Rc::new(tree);
        self.root = result.clone();
        Ok(result)
    }
}
impl TokenWriter for Encoder {
    type Error = TokenWriterError;
    type Statistics = u32; // Ignored for the time being.
    type Tree = Rc<SubTree>;
    type Data = Vec<u8>;

    fn bool(&mut self, data: Option<bool>) -> Result<Self::Tree, Self::Error> {
        self.register(SubTree::Bool(data))
    }

    fn float(&mut self, data: Option<f64>) -> Result<Self::Tree, Self::Error> {
        self.register(SubTree::Float(data))
    }

    fn string(&mut self, data: Option<&str>) -> Result<Self::Tree, Self::Error> {
        self.register(SubTree::String(data.map(str::to_string)))
    }

    fn untagged_tuple(&mut self, _data: &[Self::Tree]) -> Result<Self::Tree, Self::Error> {
        unimplemented!()
    }

    fn tagged_tuple(&mut self, tag: &str, items: &[(&str, Self::Tree)]) -> Result<Self::Tree, Self::Error> {
        self.register(SubTree::Node {
            name: tag.to_string(),
            children: items.iter().map(|(attribute, tree)| {
                (attribute.to_string(), tree.clone())
            }).collect()
        })
    }

    fn list(&mut self, items: Vec<Self::Tree>) -> Result<Self::Tree, Self::Error> {
        self.register(SubTree::List(items))
    }

    fn offset(&mut self) -> Result<Self::Tree, Self::Error> {
        // FIXME: We'll want to build a forest and put skippable stuff after the rest.
        unimplemented!()
    }

    fn done(self) -> Result<(Self::Data, Self::Statistics), Self::Error> {
        use xml_rs::writer::*;
        let mut buf = vec![];
        {
            let mut writer = EmitterConfig::new().perform_indent(true).create_writer(&mut buf);
            self.root.write(&mut writer).unwrap();
        }
        Ok((buf, 0))
    }
}
