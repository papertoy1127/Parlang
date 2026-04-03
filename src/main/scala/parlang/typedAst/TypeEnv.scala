package parlang.typedAst

import parlang.ast.*
import parlang.error.{LookupError, ParError, UnknownTypeBindingError}

trait SymbolInfo {
  def ty: Type
  def resolvedName: String
}

case class VarInfo(ty: Type, resolvedName: String) extends SymbolInfo
case class FuncInfo(ty: Type, resolvedName: String, paramTy: Type, retTy: Type) extends SymbolInfo

class TypeEnv(val localScope: Map[String, List[SymbolInfo]], val parent: Option[TypeEnv]) {
  def enterScope(): TypeEnv = new TypeEnv(Map.empty, Some(this))

  private def resolveName(id: String, signature: Type) = s"$id<>$signature"

  def bindVar(name: String, ty: Type, node: Ast): Either[ParError, (TypeEnv, VarInfo)] = {
    if (!ty.isKnown) return Left(UnknownTypeBindingError(name, ty, node))
    val info = VarInfo(ty, name)
    val updatedScope = localScope + (name -> (info :: Nil))
    Right(new TypeEnv(updatedScope, parent), info)
  }

  def bindFunc(name: String, ty: FuncTy, node: Ast): Either[ParError, (TypeEnv, FuncInfo)] = {
    if (!ty.isKnown) return Left(UnknownTypeBindingError(name, Type.single(ty), node))
    val resolvedName = resolveName(name, Type.single(ty))
    val info = FuncInfo(Type.single(ty), resolvedName, ty.param, ty.result)
    localScope.getOrElse(name, Nil) match {
      case VarInfo(_, _) :: Nil | Nil =>
        Right(new TypeEnv(localScope + (name -> (info :: Nil)), parent), info)
      case list =>
        Right(new TypeEnv(localScope + (name -> (info :: list.filter(_.resolvedName != resolvedName))), parent), info)
    }
  }

  def lookup(name: String): List[SymbolInfo] = {
    localScope.get(name) match {
      case Some(list) => list
      case None => parent.map(_.lookup(name)).getOrElse(Nil)
    }
  }
}