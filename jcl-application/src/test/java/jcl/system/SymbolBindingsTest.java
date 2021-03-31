package jcl.system;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import jcl.compiler.function.CompileFileResult;
import jcl.compiler.function.InternalCompile;
import jcl.compiler.function.InternalEval;
import jcl.compiler.function.InternalLoad;
import jcl.compiler.sa.BootstrapExpanders;
import jcl.lang.IntegerStruct;
import jcl.lang.KeywordStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.StringInputStreamStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.reader.InternalRead;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class SymbolBindingsTest {

	private static Path tmpDir;

	@BeforeAll
	static void setup() throws IOException {
		BootstrapSymbols.bootstrap();
		BootstrapFunctions.bootstrap();
		BootstrapExpanders.bootstrap();

		final PathnameStruct sourceFile = PathnameStruct.toPathname(
				"src/main/lisp/jcl/compiler/base-macro-lambdas.lisp"
		);

		final LispStruct pathnameName = sourceFile.pathnameName();
		final LispStruct pathnameType = StringStruct.toLispString("jar");

		tmpDir = Files.createTempDirectory(Paths.get(System.getProperty("user.dir")), "tests_");

		// TODO: Can we fix Pathnames needing to have a trailing "/" for determining directory or file???
		final PathnameStruct destDirectory = PathnameStruct.toPathname(tmpDir.toFile().getAbsolutePath() + "/");
		final PathnameStruct newSourceFile = PathnameStruct.toPathname(
				destDirectory.pathnameHost(),
				destDirectory.pathnameDevice(),
				destDirectory.pathnameDirectory(),
				pathnameName,
				pathnameType,
				destDirectory.pathnameVersion()
		);

		final CompileFileResult compileFileResult = InternalCompile.compileFile(
				sourceFile,
				newSourceFile,
				TStruct.INSTANCE,
				TStruct.INSTANCE,
				CommonLispSymbols.DEFAULT_KEYWORD
		);
		InternalLoad.load(
				compileFileResult.getOutputTruename(),
				TStruct.INSTANCE,
				TStruct.INSTANCE,
				TStruct.INSTANCE,
				CommonLispSymbols.DEFAULT_KEYWORD
		);

		GlobalPackageStruct.COMMON_LISP.findSymbol("DEFUN");
	}

	@AfterAll
	static void cleanup() throws IOException {
		if (tmpDir != null) {
			try (final DirectoryStream<Path> ds = Files.newDirectoryStream(tmpDir)) {
				for (final Path file : ds) {
					Files.delete(file);
				}
				Files.delete(tmpDir);
			}
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
		final String setSymbolValue = "($setValue1 'a 1)";

		StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(setSymbolValue),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(setSymbolValue.length())
		);
		LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		InternalEval.eval(whatRead);

		final String retrieveSymbolValue = "($getValue 'a)";
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
		String test = "($getValue :any-keyword)";
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

		test = "($getValue 'nil)";
		stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(test),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(test.length())
		);
		whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		value = InternalEval.eval(whatRead);

		assertThat(value).satisfies(NILStruct.INSTANCE::eq);
		test = "($getValue '())";
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
				  ($getValue 'a))
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
				  ($getValue 'a))
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
				  ($getValue 'a))
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
				  ($getValue 'a))
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
				  ($setValue 'a 3)
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
				  (let ((b ($getValue 'a)))
				    ($setValue 'a 5)
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
		String test = "($setValue 'x 6)";
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
		String test = "($setValue 'x 6)";
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
