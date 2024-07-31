package esmeta.analyzer.domain

import esmeta.analyzer.Analyzer
import enumv.Decl

type Self = Decl & Analyzer

trait Decl
  extends AValueDecl
  with AbsRefTargetDecl
  with DomainDecl
  with FlatDomainDecl
  with OptionDomainDecl
  with SetDomainDecl
  with SimpleDomainDecl
  with uninit.Decl
  with astValue.Decl
  with bigInt.Decl
  with bool.Decl
  with clo.Decl
  with codeUnit.Decl
  with comp.Decl
  with enumv.Decl
  with cont.Decl
  with heap.Decl
  with math.Decl
  with infinity.Decl
  with nt.Decl
  with nullv.Decl
  with number.Decl
  with obj.Decl
  with part.Decl
  with pureValue.Decl
  with ret.Decl
  with simpleValue.Decl
  with state.Decl
  with str.Decl
  with undef.Decl
  with value.Decl {
  self: Self =>
}
