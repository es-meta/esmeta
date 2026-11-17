"""Exhaustive classification of ESMeta's metalanguage nodes (ES2025).

Every node kind of esmeta.lang (Step, Expression, Condition, Reference, with
operators/forms) is put in one category by how PolyfillGenerator compiles it.
Counts come from census-nodes.json (`esmeta.opcount.AllBuiltins` with
ParseAlgs.Kinds, which also tallies references and keeps nodes under Assert
steps apart, since the generator skips Assert steps).

  all  = occurrences over the 952 algorithms reached by the 501 built-ins
         (each algorithm once; same exclusions as occurrences.py)
  in80 = occurrences inside the closure of the 80 built-ins DynaJS extracts
         (stopping at the hand-written models, as in esmeta_nodes.py)

Fails if a node kind seen in the census has no category.
usage: python3 exhaustive_nodes.py [census-nodes.json] [NODES.md]
"""
import json, collections, re, sys, os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

HERE = os.path.dirname(os.path.abspath(__file__))
CENSUS = sys.argv[1] if len(sys.argv) > 1 else 'census-nodes.json'
OUT = sys.argv[2] if len(sys.argv) > 2 else 'NODES.md'
d = json.load(open(CENSUS))
A, B = d['algorithms'], d['builtins']
CODE = {'eval', 'Function', 'AsyncFunction', 'GeneratorFunction',
        'AsyncGeneratorFunction'}
s = lambda b: b.replace('INTRINSICS.', '')
B = {s(b): cl for b, cl in B.items()
     if not s(b).startswith('yet:') and s(b) not in CODE}
reach = collections.defaultdict(set)
for b, cl in B.items():
    for a in cl: reach[a].add(b)

manual = set(open(os.path.join(HERE, 'aos_manual.txt')).read().split()) | {
    'ToInteger', 'ToInt32', 'SameValue', 'Type', 'Invoke',
    'SpeciesConstructor', 'OrdinaryCreateFromConstructor'}
E = set()
for b in ['INTRINSICS.' + x for x in open(os.path.join(HERE, 'builtins.txt')).read().split()]:
    todo = [b]
    while todo:
        n = todo.pop()
        if n in E: continue
        E.add(n); todo += [c for c in A[n]['callees'] if c not in manual]

tot, ein, algs, bis = (collections.Counter(), collections.Counter(),
                       collections.Counter(), collections.defaultdict(set))
for a in reach:
    for k, v in A[a]['kinds'].items():
        tot[k] += v; algs[k] += 1; bis[k] |= reach[a]
        if a in E: ein[k] += v
conv, convE = collections.Counter(), collections.Counter()
for a in reach:
    for k, v in A[a]['conv'].items():
        conv[k] += v
        if a in E: convE[k] += v

# ---------------------------------------------------------------------------
# categories
H = 'H'   # Table I hook
D = 'D'   # enters through `default` (constants, intrinsics, closures)
C = 'C'   # control / binding / structure: computes no value
I = 'I'   # invokes another algorithm (extracted recursively, or get/set/call)
P = 'P'   # passes its operand through: identity on the JS representation
R = 'R'   # spec-internal Record / execution-context machinery, not an ES value
L = 'L'   # List structure without a Table I hook
G = 'G'   # value computation without a Table I hook
X = 'X'   # generator cannot compile (`???`); a model reaching it is not generated
Y = 'Y'   # unparsed prose (Yet): manual rule or `default`
CAT = {H: 'Table I hook', D: 'default', C: '제어·구조', I: '알고리즘 호출',
       P: '값 그대로 통과', R: '명세 내부 Record·문맥', L: 'List 구조 (hook 없음)',
       G: '값 계산 (hook 없음)', X: '생성기 미지원 (???)', Y: '파싱 안 됨 (Yet)'}

# key -> (category, generator output / Table I op, note)
K = {
  # ---- steps
  'step:LetStep': (C, 'var x = e', ''),
  'step:SetStep': (C, 'x = e', ''),
  'step:SetAsStep': (X, '???', ''),
  'step:SetEvaluationStateStep': (X, '???', ''),
  'step:PerformStep': (C, 'e;', ''),
  'step:InvokeShorthandStep': (X, '???', ''),
  'step:AppendStep': (H, 'append', ''),
  'step:PrependStep': (H, 'prepend', ''),
  'step:InsertStep': (X, '???', 'List 맨 앞에 삽입 (prepend와 같은 뜻, eval의 EvalDeclarationInstantiation만)'),
  'step:AddStep': (X, '???', 'CharSet에 원소 추가 (RegExp, MaybeSimpleCaseFolding)'),
  'step:ReplaceStep': (H, 'replace', 'List 원소 교체'),
  'step:RemoveStep:First(None)': (H, 'remove', 'List 첫 원소 제거'),
  'step:RemoveStep:Last(None)': (H, 'remove', 'List 마지막 원소 제거'),
  'step:RemoveStep:First(Some(_n_))': (X, '???', '앞에서 n개 제거'),
  'step:RemoveStep:Element(_cell_)': (X, '???', '특정 원소 제거'),
  'step:RemoveStep:Element(_waiterRecord_)': (X, '???', '특정 원소 제거'),
  'step:PushContextStep': (X, '???', ''),
  'step:SuspendStep': (C, '(no-op)', ''),
  'step:RemoveContextStep': (X, '???', ''),
  'step:AssertStep': (C, '(skipped)', '안의 노드는 assert/로 따로 셈'),
  'step:IfStep': (C, 'if', ''),
  'step:RepeatStep': (C, 'while', ''),
  'step:ForEachStep': (C, 'for...of', 'List 원소 순회'),
  'step:ForEachIntegerStep': (H, 'range', ''),
  'step:ForEachOwnPropertyKeyStep': (X, '???', ''),
  'step:ForEachParseNodeStep': (X, '???', ''),
  'step:ReturnStep': (C, 'return', ''),
  'step:ThrowStep': (C, 'throw', ''),
  'step:ResumeStep': (X, '???', ''),
  'step:ResumeEvaluationStep': (X, '???', ''),
  'step:ResumeTopContextStep': (X, '???', ''),
  'step:NoteStep': (C, '(skipped)', ''),
  'step:BlockStep': (C, '{...}', ''),
  'step:YetStep': (Y, 'polyfill-rule / throw', ''),
  'step:SetFieldsWithIntrinsicsStep': (X, '???', ''),
  'step:PerformBlockStep': (X, '???', ''),
  'step:WrappedTryCatchStep': (C, 'try/catch', ''),
  'step:TaggedStep': (C, '(inner step)', ''),
  'step:MetaStep': (X, '???', ''),
  # ---- expressions
  'expr:StringConcatExpression': (H, 'concatenate', ''),
  'expr:ListConcatExpression': (H, 'listConcat', 'List 연결'),
  'expr:ListCopyExpression': (L, 'L.slice()', 'List 복사'),
  'expr:RecordExpression': (R, '{...}', 'Record 생성'),
  'expr:LengthExpression': (H, 'length', '참조가 아닌 operand는 ???'),
  'expr:SubstringExpression': (H, 'substring', ''),
  'expr:TrimExpression': (H, 'trim', ''),
  'expr:NumberOfExpression': (H, 'length', 'List 원소 개수 (e94b139)'),
  'expr:IntrinsicExpression': (D, 'default (operand 없음)', ''),
  'expr:SourceTextExpression': (X, '???', 'parse node 대상'),
  'expr:CoveredByExpression': (X, '???', ''),
  'expr:GetItemsExpression': (X, '???', ''),
  'expr:List:LiteralSyntax': (L, '[a, b]', 'List 리터럴'),
  'expr:List:SoleElement': (L, '[a]', 'List 리터럴'),
  'expr:List:EmptyList': (L, '[]', '빈 List'),
  'expr:List:IntRange': (H, 'range', ''),
  'expr:XRefExpression': (I, 'AO 함수 참조 / 인자 개수', ''),
  'expr:SoleElementExpression': (X, '???', ''),
  'expr:CodeUnitAtExpression': (H, 'codeUnitAt', ''),
  'expr:StringExpression': (P, 'e', ''),
  'expr:YetExpression': (Y, 'polyfill-rule / throw', ''),
  'expr:InvokeAbstractOperationExpression': (I, 'AO__f($, ...)', ''),
  'expr:InvokeNumericMethodExpression': (I, 'T__f($, ...)', ''),
  'expr:InvokeAbstractClosureExpression': (I, 'f(...)', ''),
  'expr:InvokeMethodExpression': (I, 'r.m(...)', 'Record 메서드 (환경 레코드 등)'),
  'expr:InvokeSyntaxDirectedOperationExpression': (X, '???', ''),
  'expr:AbstractClosureExpression': (D, 'default (operand = 캡처 변수)', ''),
  'expr:MetaExpression': (X, '???', ''),
  'expr:ClampExpression': (H, 'clamp', ''),
  'expr:Bitwise:bitwise AND operation': (H, 'bitwiseAND', ''),
  'expr:Bitwise:bitwise inclusive OR operation': (H, 'bitwiseOR', ''),
  'expr:Bitwise:bitwise exclusive OR (XOR) operation': (H, 'bitwiseXOR', ''),
  # ---- calc expressions
  'calc:ReturnIfAbruptExpression': (P, 'e', ''),
  'calc:ReferenceExpression': (P, 'e', ''),
  'calc:Binary:+': (H, 'add', ''),
  'calc:Binary:-': (H, 'subtract', ''),
  'calc:Binary:×': (H, 'multiply', ''),
  'calc:Binary:/': (H, 'divide', ''),
  'calc:Binary:modulo': (H, 'modulo', ''),
  'calc:Unary:-': (H, 'negate', ''),
  'calc:ExponentiationExpression': (H, 'exponentiate', ''),
  'calc:MathFunc:max': (H, 'max', ''), 'calc:MathFunc:min': (H, 'min', ''),
  'calc:MathFunc:abs': (H, 'abs', ''), 'calc:MathFunc:floor': (H, 'floor', ''),
  'calc:MathFunc:truncate': (H, 'truncate', ''),
  'calc:ConversionExpression': (None, '(아래 conv 표)', ''),
  # literals
  'calc:ThisLiteral': (C, 'this 매개변수', ''),
  'calc:ThisParseNodeLiteral': (X, '???', ''),
  'calc:NewTargetLiteral': (D, 'default', ''),
  'calc:HexLiteral': (D, 'default', ''),
  'calc:CodeLiteral': (D, 'default', ''),
  'calc:GrammarSymbolLiteral': (X, '???', ''),
  'calc:NonterminalLiteral': (X, '???', ''),
  'calc:EnumLiteral': (D, 'default', ''),
  'calc:StringLiteral': (D, 'default', ''),
  'calc:FieldLiteral': (R, '"name" (Record 키)', ''),
  'calc:ProductionLiteral': (X, '???', ''),
  'calc:ErrorObjectLiteral': (C, 'new TypeError()', 'throw 대상'),
  'calc:SymbolLiteral': (D, 'default', ''),
  'calc:PositiveInfinityMathValueLiteral': (D, 'default', ''),
  'calc:NegativeInfinityMathValueLiteral': (D, 'default', ''),
  'calc:DecimalMathValueLiteral': (D, 'default', ''),
  'calc:MathConstantLiteral': (X, '???', 'π 등'),
  'calc:NumberLiteral': (D, 'default', ''),
  'calc:BigIntLiteral': (D, 'default', ''),
  'calc:TrueLiteral': (D, 'default', ''), 'calc:FalseLiteral': (D, 'default', ''),
  'calc:UndefinedLiteral': (D, 'default', ''), 'calc:NullLiteral': (D, 'default', ''),
  # ---- conditions
  'cond:ExpressionCondition': (C, 'condition(...)', ''),
  'cond:TypeCheckCondition': (H, 'isType / isInteger', ''),
  'cond:HasFieldCondition': (R, '"f" in r / instanceof', 'Record 필드·내부 슬롯 유무'),
  'cond:HasBindingCondition': (X, '???', ''),
  'cond:ProductionCondition': (X, '???', ''),
  'cond:Predicate:finite': (H, 'isFinite', ''),
  'cond:Predicate:present': (C, '!== undefined', '선택 인자 유무'),
  'cond:Predicate:empty': (H, 'isEmpty', 'List가 비었는지 (분기 번호 없음)'),
  'cond:Predicate:a normal completion': (X, '???', ''),
  'cond:Predicate:a return completion': (X, '???', ''),
  'cond:Predicate:a throw completion': (X, '???', ''),
  'cond:Predicate:an abrupt completion': (X, '???', ''),
  'cond:Predicate:never an abrupt completion': (X, '???', ''),
  'cond:Predicate:a fully populated Property Descriptor': (X, '???', ''),
  'cond:IsAreCondition': (H, 'is / isNaN', ''),
  'cond:Binary:=': (H, 'is', ''), 'cond:Binary:≠': (H, 'isNot', ''),
  'cond:Binary:<': (H, 'lessThan', ''), 'cond:Binary:≤': (H, 'lessThanEqual', ''),
  'cond:Binary:>': (H, 'greaterThan', ''), 'cond:Binary:≥': (H, 'greaterThanEqual', ''),
  'cond:Binary:the same sequence of code units as': (H, 'is', ''),
  'cond:InclusiveIntervalCondition': (H, 'greaterThanEqual + lessThanEqual', ''),
  'cond:Contains:Expr': (H, 'contains', ''),
  'cond:Contains:WhoseField': (X, '???', ''),
  'cond:Contains:SuchThat': (X, '???', ''),
  'cond:Compound:and': (C, '&&', ''), 'cond:Compound:or': (C, '||', ''),
  'cond:Compound:then': (X, '???', 'implication'),
  'cond:MetaCondition': (X, '???', ''),
  # ---- references
  'ref:Variable': (P, 'x', ''),
  'ref:Access:': (R, 'r["f"]', 'Record 필드'),
  'ref:Access:BoxedData': (H, 'primitiveValue', '박싱된 값의 [[StringData]] 등'),
  'ref:Access:Component': (R, 'r["f"]', ''),
  'ref:ValueOf': (P, 'e', ''),
  'ref:IntrinsicField': (X, '???', ''),
  'ref:IndexLookup': (L, 'L[i]', 'List 원소 읽기'),
  'ref:BindingLookup': (X, '???', ''),
  'ref:NonterminalLookup': (X, '???', ''),
  'ref:PositionalElement:first': (L, 'L[0]', 'List 첫 원소'),
  'ref:PositionalElement:last': (X, '???', 'List 마지막 원소'),
  'ref:IntrinsicObject': (X, '???', ''),
  'ref:RunningExecutionContext': (R, 'this', ''),
  'ref:SecondExecutionContext': (X, '???', ''),
  'ref:CurrentRealmRecord': (R, 'globalThis', ''),
  'ref:ActiveFunctionObject': (R, '_self', ''),
  'ref:AgentRecord': (X, '???', ''),
  'ref:MetaReference': (X, '???', ''),
}
# Math function operators of MathOpExpression
for k in [k for k in tot if k.startswith('expr:MathOp:')]:
    K[k] = (H, 'Math hook (negate/exponentiate/sqrt …)', '')

CONV = {  # ConversionExpression by operator (conv tally keys)
  'ℝ': (P, 'e', 'ToMath, 기호 형식 ℝ(x)'),
  'ℝ (text)': (H, 'numericValue', 'ToMath, 텍스트 형식 "the numeric value of X" (charCodeAt, IsLessThan 포함)'),
  '𝔽': (P, 'e', 'ToNumber. JS에서는 같은 double'),
  'implementation-approximated Number': (P, 'e', 'ToApproxNumber'),
  'ℤ': (H, 'toBigInt', 'ToBigInt. 콜백이 없으면 값 그대로'),
  'code unit whose numeric': (H, 'fromCodeUnit', 'ToCodeUnit'),
}

base = lambda k: k[len('assert/'):] if k.startswith('assert/') else k
missing = [k for k in tot if base(k) not in K]
if missing:
    sys.exit('unclassified: ' + ', '.join(sorted(missing)))

rows = []
for k in sorted(set(K) | {base(k) for k in tot}):
    cat, out, note = K[k]
    if cat is None: continue
    a = 'assert/' + k
    rows.append((cat, k, tot[k], algs[k], len(bis[k]), ein[k], tot[a], ein[a], out, note))

with open(OUT, 'w') as f:
    w = lambda *x: print(*x, file=f)
    w('# ESMeta 명세 언어 노드 전수 분류 (ES2025)\n')
    w(f'- 대상: built-in {len(B)}개가 닿는 알고리즘 {len(reach)}개 (각 한 번). '
      f'80개 = DynaJS가 추출하는 built-in의 closure (알고리즘 {len(E & set(reach))}개).')
    w('- 분류 기준: `PolyfillGenerator`(esmeta `dynajs-major-revision`)가 그 노드를 어떻게 컴파일하는지.')
    w('- `assert` 열: Assert step 안의 등장. 생성기가 Assert step을 건너뛰므로 모델에 들어가지 않음.')
    w('- 재현: `ESMETA_HOME=~/code/esmeta sbt "runMain esmeta.opcount.AllBuiltins census-nodes.json"` 후 '
      '`python3 exhaustive_nodes.py`.\n')
    w('## 분류별 요약 (Assert 밖)\n')
    w('| 분류 | 노드 종류 | 등장 (전체) | 등장 (80개) |')
    w('|---|---:|---:|---:|')
    for c in CAT:
        rs = [r for r in rows if r[0] == c]
        w(f'| {c} {CAT[c]} | {len(rs)} | {sum(r[2] for r in rs)} | {sum(r[5] for r in rs)} |')
    for c in CAT:
        rs = [r for r in rows if r[0] == c]
        if not rs: continue
        w(f'\n## {c}. {CAT[c]}\n')
        w('| 노드 | 등장 (알고리즘) | 닿는 built-in | 80개 | assert (80개) | 생성 코드 | 비고 |')
        w('|---|---:|---:|---:|---:|---|---|')
        for _, k, t, al, bi, e, ta, tea, out, note in rs:
            w(f'| `{k}` | {t} ({al}) | {bi} | {e} | {ta} ({tea}) | `{out}` | {note} |')
    w('\n## ConversionExpression (연산자별)\n')
    w('| 연산자 | 분류 | 등장 | 80개 | 생성 코드 | 비고 |')
    w('|---|---|---:|---:|---|---|')
    for op, (c, out, note) in CONV.items():
        w(f'| {op} | {c} | {conv[op]} | {convE[op]} | `{out}` | {note} |')
print('wrote NODES.md;', len(rows), 'node kinds')
