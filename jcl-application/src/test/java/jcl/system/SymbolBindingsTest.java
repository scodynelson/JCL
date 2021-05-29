package jcl.system;

import jcl.compiler.function.InternalEval;
import jcl.lang.IntegerStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.StringInputStreamStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.reader.InternalRead;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import static org.assertj.core.api.Assertions.assertThat;

@ExtendWith(LoadLispFilesExtension.class)
class SymbolBindingsTest {

	@Test
	void testLambda() {
		final String setVariable = "(setq x 5)";
		StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(setVariable),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(setVariable.length())
		);
		LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		final String definition = "((lambda (y) ($add x y)) 3)";

		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(definition),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(definition.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		final LispStruct value = InternalEval.eval(whatRead);

		assertThat(value).satisfies(IntegerStruct.EIGHT::eql);
	}

	@Test
	@Disabled
	void testFunctionClosure() {
		final String definition = "(defun adder (x) (function (lambda (y) ($add x y))))";

		StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(definition),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(definition.length())
		);
		LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		final String setVariable = "(setq add3 (adder 3))";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(setVariable),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(setVariable.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		final String functionCall = "(funcall add3 5)";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(functionCall),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(functionCall.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		final LispStruct value = InternalEval.eval(whatRead);

		assertThat(value).satisfies(IntegerStruct.EIGHT::eql);
	}

	@Test
	void testSymbolValue() {
		final String setSymbolValue = "($setfSymbolValue 'a 1)";

		StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(setSymbolValue),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(setSymbolValue.length())
		);
		LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		final String retrieveSymbolValue = "($symbolValue 'a)";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(retrieveSymbolValue),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(retrieveSymbolValue.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		final LispStruct value = InternalEval.eval(whatRead);

		assertThat(value).satisfies(IntegerStruct.ONE::eql);
	}

	@Test
	void testSymbolValue_SpecialSymbols() {
		String test = "($symbolValue :any-keyword)";
		StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		LispStruct value = InternalEval.eval(whatRead);

		assertThat(value).isInstanceOf(KeywordStruct.class);
		final KeywordStruct result = (KeywordStruct) value;
		assertThat(result.getName()).isEqualToIgnoringCase("any-keyword");

		test = "($symbolValue 'nil)";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);

		assertThat(value).satisfies(NILStruct.INSTANCE::eq);
		test = "($symbolValue '())";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);

		assertThat(value).satisfies(NILStruct.INSTANCE::eq);
	}

	@Test
	void testSymbolValue_CannotSeeLexical() {
		String test = """
				(let ((a 2))
				  ($symbolValue 'a))
										   """;
		StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		LispStruct value = InternalEval.eval(whatRead);
		assertThat(value).satisfies(IntegerStruct.ONE::eql);

		test = """
				(let ((a 2))
				  (setq a 3)
				  ($symbolValue 'a))
										   """;
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);
		assertThat(value).satisfies(IntegerStruct.ONE::eql);
	}

	@Test
	void testSymbolValue_CanSeeDynamic() {
		String test = """
				(let ((a 2))
				  (declare (special a))
				  ($symbolValue 'a))
										   """;
		StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		LispStruct value = InternalEval.eval(whatRead);
		assertThat(value).satisfies(IntegerStruct.TWO::eql);

		test = """
				(let ((a 2))
				  (declare (special a))
				  (setq a 3)
				  ($symbolValue 'a))
										   """;
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);
		assertThat(value).satisfies(IntegerStruct.THREE::eql);

		test = """
				(let ((a 2))
				  ($setfSymbolValue 'a 3)
				  a)
										   """;
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);
		assertThat(value).satisfies(IntegerStruct.TWO::eql);

		test = "a";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);
		assertThat(value).satisfies(IntegerStruct.THREE::eql);

		test = """
				(let ((a 4))
				  (declare (special a))
				  (let ((b ($symbolValue 'a)))
				    ($setfSymbolValue 'a 5)
				    (values a b)))
										     """;
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);

		final ListStruct expected = ListStruct.toLispList(IntegerStruct.FIVE, IntegerStruct.FOUR);
		assertThat(value).satisfies(expected::equal);

		test = "a";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);
		assertThat(value).satisfies(IntegerStruct.THREE::eql);
	}

	@Test
	void testSpecialBindings() {
		String test = """
				(defun declare-eg (y)
				  (declare (special y))
				  (let ((y t))
				       (list y
				             (locally (declare (special y)) y))))
								""";
		StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		test = "(declare-eg nil)";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		final LispStruct value = InternalEval.eval(whatRead);

		final ListStruct expected = ListStruct.toLispList(TStruct.INSTANCE, NILStruct.INSTANCE);
		assertThat(value).satisfies(expected::equal);
	}

	@Test
	void testSpecialBindings_SymbolValue() {
		String test = "($setfSymbolValue 'x 6)";
		StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		test = "(setq holder nil)";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		test = """
				(defun + (number &rest numbers)
				  "Returns the sum of numbers, performing any necessary type conversions in the process. If no numbers are supplied, 0 is returned."
				  (declare (system::%java-class-name "jcl.numbers.functions.Add"))
				  (ext:jinvoke-static
				    (ext:jmethod "add" (ext:jclass "jcl.lang.NumberStruct")
				                 (ext:jclass "java.util.List"))
				    (ext:jinvoke (ext:jmethod "toJavaList" (ext:jclass "jcl.lang.ListStruct"))
				                 (cons number numbers))))
								""";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		test = """
				(defun foo (x)
				  (setq holder (cons x holder))
				  (let ((x (+ x 1)))
				    (declare (special x))
				    (bar))
				  (setq holder (cons (+ x 1) holder))
				  holder)
								""";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		test = """
				(defun bar ()
				  (setq holder
				        (cons (locally (declare (special x)) x)
				              holder)))
								""";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		test = "(foo 10)";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		LispStruct value = InternalEval.eval(whatRead);

		final ListStruct expected = ListStruct.toLispList(
				IntegerStruct.TEN,
				IntegerStruct.ELEVEN,
				IntegerStruct.ELEVEN
		);
		assertThat(value).satisfies(expected::equal);

		test = "x";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);
		assertThat(value).satisfies(IntegerStruct.SIX::eql);
	}

	@Test
	void testSpecialBindings_Shadowing() {
		String test = "($setfSymbolValue 'x 6)";
		StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		test = """
				(defun bar (x y)
				  (let ((old-x x)
				        (x y))
				    (declare (special x))
				    (list old-x x)))
								""";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		test = "(bar 'first 'second)";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		final LispStruct value = InternalEval.eval(whatRead);

		final ListStruct expected = ListStruct.toLispList(
				SymbolStruct.toLispSymbol("FIRST"),
				SymbolStruct.toLispSymbol("SECOND")
		);
		assertThat(value).satisfies(expected::equalp);
	}

	/*
The reference to *foo* in the first line of this example is not special even though there is a special declaration in the second line.
 (declaim (special prosp)) =>  implementation-dependent
 (setq prosp 1 reg 1) =>  1
 (let ((prosp 2) (reg 2))         ;the binding of prosp is special
    (set 'prosp 3) (set 'reg 3)   ;due to the preceding proclamation,
    (list prosp reg))             ;whereas the variable reg is lexical
=>  (3 2)
 (list prosp reg) =>  (1 3)

 (declaim (special x))          ;x is always special.
 (defun example (x y)
   (declare (special y))
   (let ((y 3) (x (* x 2)))
     (print (+ y (locally (declare (special y)) y)))
     (let ((y 4)) (declare (special y)) (foo x)))) =>  EXAMPLE
	 */
}
