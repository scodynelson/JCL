package jcl.system;

import jcl.compiler.environment.Environment;
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
import jcl.lang.condition.exception.UnboundVariableException;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.reader.InternalRead;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.fail;

@ExtendWith(LoadLispFilesExtension.class)
class SymbolBindingsTest {

	@BeforeEach
	void clearNullEnvironment() {
		Environment.NULL.getLexicalSymbolBindings().clear();
		Environment.NULL.getDynamicSymbolBindings().clear();
		Environment.NULL.getLexicalFunctionBindings().clear();
		Environment.NULL.getMacroFunctionBindings().clear();
		Environment.NULL.getSymbolMacroBindings().clear();

		for (final SymbolStruct symbol : GlobalPackageStruct.COMMON_LISP_USER.getExternalSymbols()) {
			symbol.setfSymbolValue(null);
		}
		for (final SymbolStruct symbol : GlobalPackageStruct.COMMON_LISP_USER.getInternalSymbols()) {
			symbol.setfSymbolValue(null);
		}
	}

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

		final String definition = "((lambda (y) (+ x y)) 3)";

		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(definition),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(definition.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		final LispStruct value = InternalEval.eval(whatRead);

		assertThat(IntegerStruct.EIGHT.eql(value)).isTrue();
	}

	@Test
	@Disabled
	void testFunctionClosure() {
		final String definition = "(defun adder (x) (function (lambda (y) (+ x y))))";

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

		assertThat(IntegerStruct.EIGHT.eql(value)).isTrue();
	}

	@Test
	void testSymbolValue() {
		final String setSymbolValue = """
				   (ext:jinvoke-interface
				     (ext:jmethod "setfSymbolValue" (ext:jclass "jcl.lang.SymbolStruct")
				                  (ext:jclass "jcl.lang.LispStruct"))
				     'a 1)
				""";

		StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(setSymbolValue),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(setSymbolValue.length())
		);
		LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		final String retrieveSymbolValue = "(symbol-value 'a)";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(retrieveSymbolValue),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(retrieveSymbolValue.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		final LispStruct value = InternalEval.eval(whatRead);

		assertThat(IntegerStruct.ONE.eql(value)).isTrue();
	}

	@Test
	void testSymbolValue_SpecialSymbols() {
		String test = "(symbol-value :any-keyword)";
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

		test = "(symbol-value 'nil)";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);

		assertThat(NILStruct.INSTANCE.eq(value)).isTrue();

		test = "(symbol-value '())";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);

		assertThat(NILStruct.INSTANCE.eq(value)).isTrue();
	}

	@Test
	void testSymbolValue_CannotSeeLexical() {
		String test = """
				(let ((a 2))
				  (symbol-value 'a))
										   """;
		StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		try {
			InternalEval.eval(whatRead);
			fail("Expected 'UnboundVariableException' to be thrown");
		} catch (final UnboundVariableException ignore) {
		}

		test = """
				(let ((a 2))
				  (setq a 3)
				  (symbol-value 'a))
										   """;
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		try {
			InternalEval.eval(whatRead);
		} catch (final UnboundVariableException ignore) {
		}
	}

	@Test
	void testSymbolValue_CanSeeDynamic() {
		String test = """
				(let ((a 2))
				  (declare (special a))
				  (symbol-value 'a))
										   """;
		StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		LispStruct value = InternalEval.eval(whatRead);
		assertThat(IntegerStruct.TWO.eql(value)).isTrue();

		test = """
				(let ((a 2))
				  (declare (special a))
				  (setq a 3)
				  (symbol-value 'a))
										   """;
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);
		assertThat(IntegerStruct.THREE.eql(value)).isTrue();

		test = """
				(let ((a 2))
				  (ext:jinvoke-interface
				      (ext:jmethod "setfSymbolValue" (ext:jclass "jcl.lang.SymbolStruct")
				                   (ext:jclass "jcl.lang.LispStruct"))
				      'a 3)
				  a)
										   """;
		/*
				(let ((a 2))
				  (setf (symbol-value 'a) 3)
				  a)
		 */
		// NOTE: 'a' is now 3 globally
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);
		assertThat(IntegerStruct.TWO.eql(value)).isTrue();

		test = "a";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);
		assertThat(IntegerStruct.THREE.eql(value)).isTrue();

		test = """
				(let ((a 4))
				  (declare (special a))
				  (let ((b (symbol-value 'a)))
				    (ext:jinvoke-interface
				      (ext:jmethod "setfSymbolValue" (ext:jclass "jcl.lang.SymbolStruct")
				                   (ext:jclass "jcl.lang.LispStruct"))
				      'a 5)
				    (list a b)))
										     """;
		/*
				(let ((a 4))
				  (declare (special a))
				  (let ((b (symbol-value 'a)))
				    (setf (symbol-value 'a) 5)
				    (list a b)))
		 */
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);

		final ListStruct expected = ListStruct.toLispList(IntegerStruct.FIVE, IntegerStruct.FOUR);
		assertThat(expected.equal(value)).isTrue();

		test = "a";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);
		assertThat(IntegerStruct.THREE.eql(value)).isTrue();
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
		assertThat(expected.equal(value)).isTrue();
	}

	@Test
	void testSpecialBindings_SymbolValue() {
		String test = """
				   (ext:jinvoke-interface
				     (ext:jmethod "setfSymbolValue" (ext:jclass "jcl.lang.SymbolStruct")
				                  (ext:jclass "jcl.lang.LispStruct"))
				     'x 6)
				""";
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
				IntegerStruct.ELEVEN,
				IntegerStruct.ELEVEN,
				IntegerStruct.TEN
		);
		assertThat(expected.equal(value)).isTrue();

		test = "x";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);
		assertThat(IntegerStruct.SIX.eql(value)).isTrue();
	}

	@Test
	void testSpecialBindings_Shadowing() {
		String test = """
				   (ext:jinvoke-interface
				     (ext:jmethod "setfSymbolValue" (ext:jclass "jcl.lang.SymbolStruct")
				                  (ext:jclass "jcl.lang.LispStruct"))
				     'x 6)
				""";
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
				GlobalPackageStruct.COMMON_LISP_USER.findSymbol("FIRST").getSymbol(),
				GlobalPackageStruct.COMMON_LISP_USER.findSymbol("SECOND").getSymbol()
		);
		assertThat(expected.equalp(value)).isTrue();
	}

	/*
The reference to *foo* in the first line of this example is not special even though there is a special declaration in the second line.
 (declaim (special prosp)) =>  implementation-dependent
 (setq prosp 1 reg 1) =>  1
 (let ((prosp 2) (reg 2))         ;the binding of prosp is special
    ()set 'prosp 3) (set 'reg 3)   ;due to the preceding proclamation,
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
