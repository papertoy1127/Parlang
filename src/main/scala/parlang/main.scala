package parlang

import parlang.ast.*
import parlang.parser.Parser
import parlang.typedAst.*
import parlang.error.ParError

// LLM generated tests

@main def runTests(): Unit = {
  // --- 1. 초기 환경 설정 (Built-in Operators) ---
  val initialEnv = TypeEnv(Map(), None)
    .bindVar("+", Type.single(FuncTy(Type.Exact(List(IntTy(), IntTy())), Type.single(IntTy()))), BuiltinBase).toOption.get._1
    .bindVar("-", Type.single(FuncTy(Type.Exact(List(IntTy(), IntTy())), Type.single(IntTy()))), BuiltinBase).toOption.get._1
    .bindVar("*", Type.single(FuncTy(Type.Exact(List(IntTy(), IntTy())), Type.single(IntTy()))), BuiltinBase).toOption.get._1
    .bindVar("/", Type.single(FuncTy(Type.Exact(List(IntTy(), IntTy())), Type.single(IntTy()))), BuiltinBase).toOption.get._1
    .bindVar("=", Type.single(FuncTy(Type.Exact(List(IntTy(), IntTy())), Type.single(BoolTy()))), BuiltinBase).toOption.get._1
    .bindVar("!=", Type.single(FuncTy(Type.Exact(List(IntTy(), IntTy())), Type.single(BoolTy()))), BuiltinBase).toOption.get._1

  // --- 2. 테스트 헬퍼 함수 ---
  def runTest(name: String, code: String): Unit = {
    println(s"\n--- [Test Case: $name] ---")
    try {
      val astList = Parser.parseProgram(code)
      println(astList)
      val result = astList.foldLeft[Either[ParError, (TypeEnv, List[TypedStmt])]](Right((initialEnv, Nil))) {
        case (Right((currEnv, acc)), stmt) =>
          TypeChecker.check(stmt, currEnv).map { case (nextEnv, typed) => (nextEnv, acc :+ typed) }
        case (Left(err), _) => Left(err)
      }

      result match {
        case Right((finalEnv, _)) =>
          println(s"✅ SUCCESS: $name")
          // 전역 환경에 새로 등록된 유저 변수들만 출력
          val userVars = finalEnv.localScope.filterNot { case (k, _) => initialEnv.localScope.contains(k) }
          userVars.foreach { case (id, infos) => println(f"  $id%-12s : ${infos.head.ty}") }

        case Left(err) =>
          println(s"❌ FAILED: $name")
          println(err.message)
      }
    } catch {
      case e: Exception => println(s"💥 SYNTAX ERROR:\n${e.getMessage}")
    }
  }

  // --- 3. 테스트 시나리오 목록 ---

  // 시나리오 1: 기본 바인딩 및 가변 인자 (Tail & Variadic)
  runTest("Basic & Variadic Pattern", """
    let (x, y, *z: *Int) := (1, 2, 3, 4, 5)
    let *all_int: *Int := (x, y, z)
  """)

  // 시나리오 2: 고차 함수와 클로저 (Currying)
  runTest("Higher-Order Functions", """
    let makeAdder := (x: Int) => (y: Int) => x + y
    let add5 := makeAdder(5)
    let ten: Int := add5(5)
  """)

  // 시나리오 3: 재귀 함수 (Fibonacci)
  runTest("Recursive Function", """
    let fib (n: Int) -> Int := {
      if (n = 0) 0
      else if (n = 1) 1
      else fib(n - 1) + fib(n - 2)
    }
    let res := fib(10)
  """)

  // 시나리오 4: 태그드 타입 (ADT 스타일)
  runTest("Tagged Types", """
    let myPoint: #(Int, Int) := #(10, 20)
    let #(px: Int, py: Int) := myPoint
  """)

  // 시나리오 5: 중첩 튜플 평탄화 (Flattening)
  runTest("Tuple Flattening", """
    let ((a, b), c, (d, *e: *Int)) := (1, 2, 3, 4, 5, 6)
    let flattened_all := (a, b, c, d, e)
  """)

  // 시나리오 6: 조건문 타입 단일화
  runTest("Conditional Unification", """
    let cond := true
    let *res: (Int, *Int) := if (cond) (1, 2, 3) else (0,)
  """)

  runTest("Error: Tuple Type Mismatch", """
    let a: Int := 10
    let b: Bool := true
    let *t: (Int, Bool, String) := (a, b, 1)
  """)

  runTest("Error: Unbound Identifier", """
    let x := 1 + undefined_var
  """)

  runTest("Error: Incompatible Branches", """
    let res := if (true) 1 else "not_a_number"
  """)

  runTest("Error: Variadic Placement", """
    let ( *bad: *Int, x: Int ) := (1, 2, 3)
  """)

  runTest("Error: Call Parameter Mismatch", """
    let f (a: Int, b: Int) -> Int := a + b
    let res := f(1)
  """)
}