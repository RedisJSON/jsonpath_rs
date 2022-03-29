use pest::{
    Parser,
};
use pest::iterators::{Pair, Pairs};
use std::cmp::Ordering;

use std::fmt::Debug;
use crate::select_value::{SelectValue, SelectValueType};

#[derive(Parser)]
#[grammar = "grammer.pest"]
pub struct JsonPathParser;

#[derive(Debug)]
pub struct Query<'i>{
    // query: QueryElement<'i>
    pub query: Pairs<'i, Rule>,
}

#[derive(Debug)]
pub struct QueryCompilationError {
    location: usize,
    message: String,
}

impl std::fmt::Display for QueryCompilationError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "Error accured on possition {}, {}", self.location, self.message)
    }
}

impl std::fmt::Display for Rule {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        match self {
            Rule::literal => write!(f, "<string>"),
            Rule::all => write!(f, "'*'"),
            Rule::full_scan => write!(f, "'..'"),
            Rule::numbers_list => write!(f, "'<number>[,<number>,...]'"),
            Rule::string_list => write!(f, "'<string>[,<string>,...]'"),
            Rule::numbers_range => write!(f, "['start:end:steps']"),
            Rule::number => write!(f, "'<number>'"),
            Rule::filter => write!(f, "'[?(filter_expression)]'"),
            _ => write!(f, "{:?}", self)
        }
    }
}

pub (crate) fn compile<'i>(s: &'i str) -> Result<Query<'i>, QueryCompilationError> {
    let q = JsonPathParser::parse(Rule::query, s);
    match q {
        Ok(q) => {
            Ok(Query{query:q})
        }
        // pest::error::Error
        Err(e) => {
            let pos = match e.location {
                pest::error::InputLocation::Pos(pos) => pos,
                pest::error::InputLocation::Span((pos, _end)) => pos,
            };
            let msg = match e.variant {
                pest::error::ErrorVariant::ParsingError {
                    ref positives,
                    ref negatives,
                } => {
                    let positives = if positives.len() > 0 {
                        Some(positives.iter().map(|v| format!("{}", v)).collect::<Vec<_>>().join(", "))
                    } else {
                        None
                    };
                    let negatives = if negatives.len() > 0 {
                        Some(negatives.iter().map(|v| format!("{}", v)).collect::<Vec<_>>().join(", "))
                    } else {
                        None
                    };

                    match (positives, negatives) {
                        (None, None) => "parsing error".to_string(),
                        (Some(p), None) => format!("expected one of the following: {}", p),
                        (None, Some(n)) => format!("unexpected tokens found: {}", n),
                        (Some(p), Some(n)) => format!("expected one of the following: {}, unexpected tokens found: {}", p, n),
                    }
                    
                }
                pest::error::ErrorVariant::CustomError { ref message } => message.clone(),
            };

            let final_msg = if pos == s.len() {
                format!("\"{} <<<<----\", {}.", s, msg)
            } else {
                format!("\"{} ---->>>> {}\", {}.", &s[..pos], &s[pos..], msg)
            };
            Err(QueryCompilationError{
                location: pos,
                message: final_msg,
            })
        }
    }
}

pub trait UserPathTracker {
    fn add_str(&mut self, s: &str);
    fn add_index(&mut self, i: usize);
    fn to_string_path(self) -> Vec<String>;
}

pub trait UserPathTrackerGenerator {
    type PT: UserPathTracker;
    fn generate(&self) -> Self::PT;
}

pub struct DummyTracker;
impl UserPathTracker for DummyTracker {
    fn add_str(&mut self, _s: &str){}
    fn add_index(&mut self, _i: usize){}
    fn to_string_path(self) -> Vec<String> {Vec::new()}
}

pub struct DummyTrackerGenerator;
impl UserPathTrackerGenerator for DummyTrackerGenerator {
    type PT = DummyTracker;
    fn generate(&self) -> Self::PT {
        DummyTracker
    }
}

#[derive(Debug, PartialEq)]
pub enum PTrackerElement {
    Key(String),
    Index(usize),
}

#[derive(Debug, PartialEq)]
pub struct PTracker {
    pub elemenets: Vec<PTrackerElement>,
}
impl UserPathTracker for PTracker {
    fn add_str(&mut self, s: &str){
        self.elemenets.push(PTrackerElement::Key(s.to_string()))
    }

    fn add_index(&mut self, i: usize){
        self.elemenets.push(PTrackerElement::Index(i))
    }

    fn to_string_path(self) -> Vec<String> {
        self.elemenets.into_iter().map(|e|{
            match e {
                PTrackerElement::Key(s) => s,
                PTrackerElement::Index(i) => i.to_string(),
            }
        }).collect()
    }
}

pub struct PTrackerGenerator;
impl UserPathTrackerGenerator for PTrackerGenerator {
    type PT = PTracker;
    fn generate(&self) -> Self::PT {
        PTracker{elemenets: Vec::new()}
    }
}

#[derive(Clone)]
enum PathTrackerElement<'i> {
    Index(usize),
    Key(&'i str),
    Root,
}

#[derive(Clone)]
struct PathTracker<'i, 'j> {
    father: Option<&'j PathTracker<'i, 'j>>,
    element: PathTrackerElement<'i>,
}

fn create_empty_trucker<'i, 'j>() -> PathTracker<'i, 'j> {
    PathTracker{
        father: None,
        element: PathTrackerElement::Root,
    }
}

fn create_str_trucker<'i, 'j>(s: &'i str, father: &'j PathTracker<'i, 'j>) -> PathTracker<'i, 'j> {
    PathTracker{
        father: Some(father),
        element: PathTrackerElement::Key(s),
    }
}

fn create_index_trucker<'i, 'j>(index: usize, father: &'j PathTracker<'i, 'j>) -> PathTracker<'i, 'j> {
    PathTracker{
        father: Some(father),
        element: PathTrackerElement::Index(index),
    }
}

enum TermEvaluationResult<'i, 'j, S:SelectValue> {
    Integer(i64),
    Float(f64),
    Str(&'i str),
    String(String),
    Value(&'j S),
    Bool(bool),
    Invalid,
}

enum CmpResult {
    Ord(Ordering),
    NotCmparable,
}

impl<'i, 'j, S:SelectValue> TermEvaluationResult<'i, 'j, S> {
    fn cmp(&self, s: &Self) -> CmpResult {
        match (self, s) {
            (TermEvaluationResult::Integer(n1), TermEvaluationResult::Integer(n2)) => {
                CmpResult::Ord(n1.cmp(n2))
            }
            (TermEvaluationResult::Float(_), TermEvaluationResult::Integer(n2)) => {
                self.cmp(&TermEvaluationResult::Float(*n2 as f64))
            }
            (TermEvaluationResult::Integer(n1), TermEvaluationResult::Float(_)) => {
                TermEvaluationResult::Float(*n1 as f64).cmp(s)
            }
            (TermEvaluationResult::Float(f1), TermEvaluationResult::Float(f2)) => {
                if *f1 > *f2 {
                    CmpResult::Ord(Ordering::Greater)
                } else if *f1 < *f2 {
                    CmpResult::Ord(Ordering::Less)
                } else {
                    CmpResult::Ord(Ordering::Equal)
                }
            }
            (TermEvaluationResult::Str(s1), TermEvaluationResult::Str(s2)) => {
                CmpResult::Ord(s1.cmp(s2))
            }
            (TermEvaluationResult::Str(s1), TermEvaluationResult::String(s2)) => {
                CmpResult::Ord((*s1).cmp(&s2))
            }
            (TermEvaluationResult::String(s1), TermEvaluationResult::Str(s2)) => {
                CmpResult::Ord((&s1[..]).cmp(s2))
            }
            (TermEvaluationResult::String(s1), TermEvaluationResult::String(s2)) => {
                CmpResult::Ord(s1.cmp(s2))
            }
            (TermEvaluationResult::Value(v), _) => {
                match v.get_type() {
                    SelectValueType::Long => {
                        TermEvaluationResult::Integer(v.get_long()).cmp(s)
                    }
                    SelectValueType::Double => {
                        TermEvaluationResult::Float(v.get_double()).cmp(s)
                    }
                    SelectValueType::String => {
                        TermEvaluationResult::Str(v.as_str()).cmp(s)
                    }
                    _ => CmpResult::NotCmparable
                }
            }
            (_, TermEvaluationResult::Value(v)) => {
                match v.get_type() {
                    SelectValueType::Long => {
                        self.cmp(&TermEvaluationResult::Integer(v.get_long()))
                    }
                    SelectValueType::Double => {
                        self.cmp(&TermEvaluationResult::Float(v.get_double()))
                    }
                    SelectValueType::String => {
                        self.cmp(&TermEvaluationResult::Str(v.as_str()))
                    }
                    _ => CmpResult::NotCmparable
                }
            }
            (TermEvaluationResult::Invalid, _) |
            (_, TermEvaluationResult::Invalid) => CmpResult::NotCmparable,
            (_, _) => CmpResult::NotCmparable
        }
    }
    fn gt(&self, s: &Self) -> bool {
        match self.cmp(s) {
            CmpResult::Ord(o) => o.is_gt(),
            CmpResult::NotCmparable => false,
        }
    }

    fn ge(&self, s: &Self) -> bool {
        match self.cmp(s) {
            CmpResult::Ord(o) => o.is_ge(),
            CmpResult::NotCmparable => false,
        }
    }

    fn lt(&self, s: &Self) -> bool {
        match self.cmp(s) {
            CmpResult::Ord(o) => o.is_lt(),
            CmpResult::NotCmparable => false,
        }
    }

    fn le(&self, s: &Self) -> bool {
        match self.cmp(s) {
            CmpResult::Ord(o) => o.is_le(),
            CmpResult::NotCmparable => false,
        }
    }

    fn eq(&self, s: &Self) -> bool {
        match (self, s){
            (TermEvaluationResult::Bool(b1), TermEvaluationResult::Bool(b2)) => {
                if *b1 == *b2 {
                    true
                } else {
                    false
                }
            }
            (TermEvaluationResult::Value(v1), TermEvaluationResult::Bool(b2)) => {
                if v1.get_type() == SelectValueType::Bool {
                    let b1 = v1.get_bool();
                    if b1 == *b2 {
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            }
            (TermEvaluationResult::Bool(_), TermEvaluationResult::Value(_)) => {
                s.eq(self)
            }
            (TermEvaluationResult::Value(v1), TermEvaluationResult::Value(v2)) => {
                if v1 == v2 {
                    true
                } else {
                    false
                }
            }
            (_, _) => {
                match self.cmp(s) {
                    CmpResult::Ord(o) => o.is_eq(),
                    CmpResult::NotCmparable => false,
               }
            }
        }
    }

    fn ne(&self, s: &Self) -> bool {
        if self.eq(s) {
            false
        } else {
            true
        }
    }

    fn re(&self, _s: &Self) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct PathCalculator<'i, UPTG: UserPathTrackerGenerator>{
    pub query: Option<&'i Query<'i>>,
    pub tracker_generator: Option<UPTG>,
}

#[derive(Debug, PartialEq)]
pub struct CalculationResult<'i, S: SelectValue, UPT: UserPathTracker> {
    pub res: &'i S,
    pub path_tracker: Option<UPT>,
}

#[derive(Debug, PartialEq)]
struct PathCalculatorData<'i, S: SelectValue, UPT: UserPathTracker> {
    results: Vec<CalculationResult<'i, S, UPT>>,
    root: &'i S
}

impl<'i, UPTG: UserPathTrackerGenerator> PathCalculator<'i, UPTG> {

    pub fn create(query: &'i Query<'i>) -> PathCalculator<'i, UPTG> {
        PathCalculator {
            query: Some(query),
            tracker_generator: None,
        }
    }

    pub fn create_with_generator(query: &'i Query<'i>, tracker_generator: UPTG) -> PathCalculator<'i, UPTG> {
        PathCalculator {
            query: Some(query),
            tracker_generator: Some(tracker_generator),
        }
    }

    fn calc_full_scan<'j:'i, 'k, 'l, S:SelectValue>(&self, pairs: Pairs<'i, Rule>, 
                                                    json: &'j S, 
                                                    path_tracker: Option<PathTracker<'l, 'k>>,
                                                    calc_data: &mut PathCalculatorData<'j, S, UPTG::PT>)
    {
        match json.get_type() {
            SelectValueType::Object => {
                if let Some(pt) = path_tracker {
                    let items = json.items().unwrap();
                    for (key, val) in items {
                        self.calc_internal(pairs.clone(), val, Some(create_str_trucker(key, &pt)), calc_data, false);
                        self.calc_full_scan(pairs.clone(), val, Some(create_str_trucker(key, &pt)), calc_data);
                    }
                } else {
                    let values = json.values().unwrap();
                    for v in values {
                        self.calc_internal(pairs.clone(), v, None, calc_data, false);
                        self.calc_full_scan(pairs.clone(), v, None, calc_data);
                    }
                }
            }
            SelectValueType::Array => {
                let values = json.values().unwrap();
                if let Some(pt) = path_tracker {
                    for (i, v) in values.enumerate() {
                        self.calc_internal(pairs.clone(), v, Some(create_index_trucker(i, &pt)), calc_data, false);
                        self.calc_full_scan(pairs.clone(), v, Some(create_index_trucker(i, &pt)), calc_data);
                    }
                } else {
                    for v in values {
                        self.calc_internal(pairs.clone(), v, None, calc_data, false);
                        self.calc_full_scan(pairs.clone(), v, None, calc_data);
                    }
                }
            }
            _ => {}
        }
    }

    fn calc_all<'j:'i, 'k, 'l, S:SelectValue>(&self, pairs: Pairs<'i, Rule>, 
                                              json: &'j S, 
                                              path_tracker: Option<PathTracker<'l, 'k>>,
                                              calc_data: &mut PathCalculatorData<'j, S, UPTG::PT>)
    {
        match json.get_type() {
            SelectValueType::Object => {
                if let Some(pt) = path_tracker {
                    let items = json.items().unwrap();
                    for (key, val) in items {
                        let new_tracker = Some(create_str_trucker(key, &pt));
                        self.calc_internal(pairs.clone(), val, new_tracker, calc_data, true);
                    }
                } else {
                    let values = json.values().unwrap();
                    for v in values {
                        self.calc_internal(pairs.clone(), v, None, calc_data, true);
                    }
                }
            }
            SelectValueType::Array => {
                let values = json.values().unwrap();
                if let Some(pt) = path_tracker {
                    for (i, v) in values.enumerate() {
                        let new_tracker = Some(create_index_trucker(i, &pt));
                        self.calc_internal(pairs.clone(), v, new_tracker, calc_data, true);
                    }
                } else {
                    for v in values {
                        self.calc_internal(pairs.clone(), v, None, calc_data, true);
                    }
                }
            }
            _ => {}
        }
    }

    fn calc_literal<'j:'i, 'k, 'l, S:SelectValue>(&self, pairs: Pairs<'i, Rule>,
                                                  curr: Pair<'i, Rule>,
                                                  json: &'j S, 
                                                  path_tracker: Option<PathTracker<'l, 'k>>,
                                                  calc_data: &mut PathCalculatorData<'j, S, UPTG::PT>)
    {
        let curr_val = json.get_key(curr.as_str());
        if let Some(e) = curr_val {
            if let Some(pt) = path_tracker {
                let new_tracker = Some(create_str_trucker(curr.as_str(), &pt));
                self.calc_internal(pairs, e, new_tracker, calc_data, true);
            } else {
                self.calc_internal(pairs, e, None, calc_data, true);
            }
            
        }
    }

    fn calc_strings<'j:'i, 'k, 'l, S:SelectValue>(&self, pairs: Pairs<'i, Rule>,
                                                  curr: Pair<'i, Rule>,
                                                  json: &'j S, 
                                                  path_tracker: Option<PathTracker<'l, 'k>>,
                                                  calc_data: &mut PathCalculatorData<'j, S, UPTG::PT>)
    {
        if let Some(pt) = path_tracker {
            for c in curr.into_inner() {
                let s = c.as_str();
                let curr_val = match c.as_rule() {
                    Rule::string_value => json.get_key(s),
                    Rule::string_value_escape_1 => json.get_key(&(s.replace("\\\\", "\\").replace("\\'", "'"))),
                    Rule::string_value_escape_2 => json.get_key(&(s.replace("\\\\", "\\").replace("\\\"", "\""))),
                    _ => panic!("{}", format!("{:?}", c))
                };
                if let Some(e) = curr_val {
                    let new_tracker = Some(create_str_trucker(s, &pt));
                    self.calc_internal(pairs.clone(), e, new_tracker, calc_data, true);
                }
            }
        } else {
            for c in curr.into_inner() {
                let s = c.as_str();
                let curr_val = match c.as_rule() {
                    Rule::string_value => json.get_key(s),
                    Rule::string_value_escape_1 => json.get_key(&(s.replace("\\\\", "\\").replace("\\\"", "\""))),
                    Rule::string_value_escape_2 => json.get_key(&(s.replace("\\\\", "\\").replace("\\'", "'"))),
                    _ => panic!("{}", format!("{:?}", c))
                };
                if let Some(e) = curr_val {
                    self.calc_internal(pairs.clone(), e, None, calc_data, true);
                }
            }
        }
    }

    fn calc_abs_index(&self, i: i64, n: usize) -> usize {
        if i >= 0 {
            (i as usize).min(n)
        } else {
            (i + n as i64).max(0) as usize
        }
    }

    fn calc_indexes<'j:'i, 'k, 'l, S:SelectValue>(&self, pairs: Pairs<'i, Rule>,
                                                  curr: Pair<'i, Rule>,
                                                  json: &'j S, 
                                                  path_tracker: Option<PathTracker<'l, 'k>>,
                                                  calc_data: &mut PathCalculatorData<'j, S, UPTG::PT>)
    {
        if json.get_type() != SelectValueType::Array {
            return;
        }
        let n = json.len().unwrap();
        if let Some(pt) = path_tracker {
            for c in curr.into_inner() {
                let i = self.calc_abs_index(c.as_str().parse::<i64>().unwrap(), n);
                let curr_val = json.get_index(i);
                if let Some(e) = curr_val {
                    let new_tracker = Some(create_index_trucker(i, &pt));
                    self.calc_internal(pairs.clone(), e, new_tracker, calc_data, true);
                }
            }
        } else {
            for c in curr.into_inner() {
                let i = self.calc_abs_index(c.as_str().parse::<i64>().unwrap(), n);
                let curr_val = json.get_index(i);
                if let Some(e) = curr_val {
                    self.calc_internal(pairs.clone(), e, None, calc_data, true);
                }
            }
        }
    }

    fn calc_range<'j:'i, 'k, 'l, S:SelectValue>(&self, pairs: Pairs<'i, Rule>,
                                                curr: Pair<'i, Rule>,
                                                json: &'j S, 
                                                path_tracker: Option<PathTracker<'l, 'k>>,
                                                calc_data: &mut PathCalculatorData<'j, S, UPTG::PT>)
    {
        if json.get_type() != SelectValueType::Array {
            return;
        }
        let n = json.len().unwrap();
        let curr = curr.into_inner().next().unwrap();
        let (start, end, step) = match curr.as_rule() {
            Rule::right_range => {
                let mut curr = curr.into_inner();
                let start = 0;
                let end = self.calc_abs_index(curr.next().unwrap().as_str().parse::<i64>().unwrap(), n);
                let step = match curr.next() {
                    Some(s) => s.as_str().parse::<usize>().unwrap(),
                    None => 1,
                };
                (start, end, step)
            }
            Rule::all_range => {
                let mut curr = curr.into_inner();
                let step = match curr.next() {
                    Some(s) => s.as_str().parse::<usize>().unwrap(),
                    None => 1,
                };
                (0, n, step)
            }
            Rule::left_range => {
                let mut curr = curr.into_inner();
                let start = self.calc_abs_index(curr.next().unwrap().as_str().parse::<i64>().unwrap(), n);
                let end = n;
                let step = match curr.next() {
                    Some(s) => s.as_str().parse::<usize>().unwrap(),
                    None => 1,
                };
                (start, end, step)
            }
            Rule::full_range => {
                let mut curr = curr.into_inner();
                let start = self.calc_abs_index(curr.next().unwrap().as_str().parse::<i64>().unwrap(), n);
                let end = self.calc_abs_index(curr.next().unwrap().as_str().parse::<i64>().unwrap(), n);
                let step = match curr.next() {
                    Some(s) => s.as_str().parse::<usize>().unwrap(),
                    None => 1,
                };
                (start, end, step)
            }
            _ => panic!("{}", format!("{:?}", curr))
        };
        
        if let Some(pt) = path_tracker {
            for i in (start..end).step_by(step) {
                let curr_val = json.get_index(i);
                if let Some(e) = curr_val {
                    let new_tracker = Some(create_index_trucker(i, &pt));
                    self.calc_internal(pairs.clone(), e, new_tracker, calc_data, true);
                }
            }
        } else {
            for i in (start..end).step_by(step) {
                let curr_val = json.get_index(i);
                if let Some(e) = curr_val {
                    self.calc_internal(pairs.clone(), e, None, calc_data, true);
                }
            }
        }
    }

    fn evaluate_single_term<'j:'i, S:SelectValue>(&self,
        term: Pair<'i, Rule>,
        json: &'j S,
        calc_data: &mut PathCalculatorData<'j, S, UPTG::PT>) -> TermEvaluationResult<'i, 'j, S>
    {
        match term.as_rule() {
            Rule::decimal => {
                if let Ok(i) = term.as_str().parse::<i64>() {
                    TermEvaluationResult::Integer(i)
                } else {
                    TermEvaluationResult::Float(term.as_str().parse::<f64>().unwrap())
                }
            }
            Rule::boolean_true => {
                TermEvaluationResult::Bool(true)
            }
            Rule::boolean_false => {
                TermEvaluationResult::Bool(false)
            }
            Rule::string_value => {
                TermEvaluationResult::Str(term.as_str())
            }
            Rule::string_value_escape_1 => {
                TermEvaluationResult::String(term.as_str().replace("\\\\", "\\").replace("\\'", "'"))
            }
            Rule::string_value_escape_2 => {
                TermEvaluationResult::String(term.as_str().replace("\\\\", "\\").replace("\\\"", "\""))
            }
            Rule::from_current => {
                match term.into_inner().next() {
                    Some(term) => {
                        let mut calc_data = PathCalculatorData{
                            results: Vec::new(),
                            root: json,
                        };
                        self.calc_internal(term.into_inner(), json, None, &mut calc_data, false);
                        if calc_data.results.len() == 1 {
                            TermEvaluationResult::Value(calc_data.results.pop().unwrap().res)
                        } else {
                            TermEvaluationResult::Invalid
                        }
                    }
                    None => TermEvaluationResult::Value(json)
                }
            }
            Rule::from_root => {
                match term.into_inner().next() {
                    Some(term) => {
                        let mut new_calc_data = PathCalculatorData{
                            results: Vec::new(),
                            root: calc_data.root,
                        };
                        self.calc_internal(term.into_inner(), calc_data.root, None, &mut new_calc_data, false);
                        if new_calc_data.results.len() == 1 {
                            TermEvaluationResult::Value(new_calc_data.results.pop().unwrap().res)
                        } else {
                            TermEvaluationResult::Invalid
                        }
                    }
                    None => TermEvaluationResult::Value(calc_data.root)
                }
            }
            _ => {
                panic!("{}", format!("{:?}", term))
            }
        }
    }

    fn evaluate_single_filter<'j:'i, S:SelectValue>(&self,
        curr: Pair<'i, Rule>,
        json: &'j S,
        calc_data: &mut PathCalculatorData<'j, S, UPTG::PT>) -> bool
    {
        let mut curr = curr.into_inner();
        let term1 = curr.next().unwrap();
        let term1_val = self.evaluate_single_term(term1, json, calc_data);
        if let Some(op) = curr.next() {
            let term2 = curr.next().unwrap();
            let term2_val = self.evaluate_single_term(term2, json, calc_data);
            match op.as_rule() {
                Rule::gt => term1_val.gt(&term2_val),
                Rule::ge => term1_val.ge(&term2_val),
                Rule::lt => term1_val.lt(&term2_val),
                Rule::le => term1_val.le(&term2_val),
                Rule::eq => term1_val.eq(&term2_val),
                Rule::ne => term1_val.ne(&term2_val),
                Rule::re => term1_val.re(&term2_val),
                _ => panic!("{}", format!("{:?}", op))
            }
        } else {
            match term1_val {
                TermEvaluationResult::Invalid => false,
                _ => true,
            }
        }
    }

    fn evaluate_filter<'j:'i, S:SelectValue>(&self,
                                             curr: Pair<'i, Rule>,
                                             json: &'j S,
                                             calc_data: &mut PathCalculatorData<'j, S, UPTG::PT>) -> bool
    {
        let mut curr = curr.into_inner();
        let first_filter = curr.next().unwrap();
        let first_result = match first_filter.as_rule() {
            Rule::single_filter => {
                self.evaluate_single_filter(first_filter, json, calc_data)
            }
            Rule::filter => {
                self.evaluate_filter(first_filter, json, calc_data)
            }
            _ => panic!("{}", format!("{:?}", first_filter))
        };

        if let Some(relation) = curr.next() {
            let relation_callback = match relation.as_rule() {
                Rule::and => {
                    |a: bool,b: bool|{a && b}
                }
                Rule::or => {
                    |a: bool,b: bool|{a || b}
                }
                _ => panic!("{}", format!("{:?}", relation))
            };
            let second_filter = curr.next().unwrap();
            let second_result = match second_filter.as_rule() {
                Rule::single_filter => {
                    self.evaluate_single_filter(second_filter, json, calc_data)
                }
                Rule::filter => {
                    self.evaluate_filter(second_filter, json, calc_data)
                }
                _ => panic!("{}", format!("{:?}", second_filter))
            };
            relation_callback(first_result, second_result)
        } else {
            first_result
        }
    }

    fn populate_path_tracker<'k, 'l>(&self, pt: &PathTracker<'l, 'k>, upt: &mut UPTG::PT) {
        if let Some(f) = pt.father {
            self.populate_path_tracker(f, upt)
        }
        match pt.element {
            PathTrackerElement::Index(i) => upt.add_index(i),
            PathTrackerElement::Key(k) => upt.add_str(k),
            PathTrackerElement::Root => {}
        }
    }

    fn generate_path<'k, 'l>(&self, pt: PathTracker<'l, 'k>) -> UPTG::PT {
        let mut upt = self.tracker_generator.as_ref().unwrap().generate();
        self.populate_path_tracker(&pt, &mut upt);
        upt
    }

    fn calc_internal<'j:'i, 'k, 'l, S:SelectValue>(&self, mut pairs: Pairs<'i, Rule>, 
                                        json: &'j S, 
                                        path_tracker: Option<PathTracker<'l, 'k>>,
                                        calc_data: &mut PathCalculatorData<'j, S, UPTG::PT>,
                                        flat_arrays_on_filter: bool)
    {
        let curr = pairs.next();
        match curr {
            Some(curr) => {
                match curr.as_rule() {
                    Rule::full_scan => {
                        self.calc_internal(pairs.clone(), json, path_tracker.clone(), calc_data, false);
                        self.calc_full_scan(pairs, json, path_tracker, calc_data)
                    }
                    Rule::all => {
                        self.calc_all(pairs, json, path_tracker, calc_data)
                    }
                    Rule::literal => {
                        self.calc_literal(pairs, curr, json, path_tracker, calc_data)
                    }
                    Rule::string_list => {
                        self.calc_strings(pairs, curr, json, path_tracker, calc_data)
                    }
                    Rule::numbers_list => {
                        self.calc_indexes(pairs, curr, json, path_tracker, calc_data)
                    }
                    Rule::numbers_range => {
                        self.calc_range(pairs, curr, json, path_tracker, calc_data)
                    }
                    Rule::filter => {
                        if flat_arrays_on_filter && json.get_type() == SelectValueType::Array {
                            /* lets expend the array, this is how most json path engines work.
                             * Pesonally, I think this if should not exists. */
                            let values = json.values().unwrap();
                            if let Some(pt) = path_tracker {
                                for (i, v) in values.enumerate() {
                                    if self.evaluate_filter(curr.clone(), v, calc_data) {
                                        let new_tracker = Some(create_index_trucker(i, &pt));
                                        self.calc_internal(pairs.clone(), v, new_tracker, calc_data, true);
                                    }
                                }
                            } else {
                                for v in values {
                                    if self.evaluate_filter(curr.clone(), v, calc_data) {
                                        self.calc_internal(pairs.clone(), v, None, calc_data, true);
                                    }
                                }
                            }
                        } else {
                            if self.evaluate_filter(curr.clone(), json, calc_data) {
                                self.calc_internal(pairs, json, path_tracker, calc_data, true);
                            }
                        }
                    }
                    Rule::EOI => {
                        calc_data.results.push(CalculationResult{
                            res: json,
                            path_tracker: match path_tracker {
                                Some(pt) => Some(self.generate_path(pt)),
                                None => None,
                            }
                        });
                    }
                    _ => panic!("{}", format!("{:?}", curr))
                }
            }
            None => {
                calc_data.results.push(CalculationResult{
                    res: json,
                    path_tracker: match path_tracker {
                        Some(pt) => Some(self.generate_path(pt)),
                        None => None,
                    }
                });
            }
        }
    }

    pub fn calc_with_paths_on_root<'j:'i, S:SelectValue>(&self, json: &'j S, root: Pair<Rule>) -> Vec<CalculationResult<'j, S, UPTG::PT>>
    {
        let mut calc_data = PathCalculatorData{
            results: Vec::new(),
            root: json,
        };
        if self.tracker_generator.is_some() {
            self.calc_internal(root.into_inner(), json, Some(create_empty_trucker()), &mut calc_data, true);
        } else {
            self.calc_internal(root.into_inner(), json, None, &mut calc_data, true);
        }
        calc_data.results.drain(..).collect()
    }
    
    pub fn calc_with_paths<'j:'i, S:SelectValue>(&self, json: &'j S) -> Vec<CalculationResult<'j, S, UPTG::PT>>
    {
        self.calc_with_paths_on_root(json, self.query.unwrap().query.clone().next().unwrap())
    }

    pub fn calc<'j:'i, S:SelectValue>(&self, json: &'j S) -> Vec<&'j S>
    {
        self.calc_with_paths(json).into_iter().map(|e| e.res).collect()
    }

    pub fn calc_paths<'j:'i, S:SelectValue>(&self, json: &'j S) -> Vec<Vec<String>>
    {
        self.calc_with_paths(json).into_iter().map(|e| {
            e.path_tracker.unwrap().to_string_path()
        }).collect()
    }
}
