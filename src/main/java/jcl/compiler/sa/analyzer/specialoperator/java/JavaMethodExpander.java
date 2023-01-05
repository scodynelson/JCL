package jcl.compiler.sa.analyzer.specialoperator.java;

import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.sa.FormAnalyzer;
import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.StringStruct;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.java.JavaClassStruct;
import jcl.lang.java.JavaMethodStruct;
import jcl.lang.statics.CommonLispSymbols;
import lombok.experimental.UtilityClass;

@UtilityClass
class JavaMethodExpander {

	// TODO: should make JavaMethod and JavaClass function calls into special operators??

	static JavaMethodStruct expandJavaMethod(final ConsStruct methodForm, final Environment environment) {
		final LispStruct first = methodForm.car();
		if (!CommonLispSymbols.JMETHOD.eql(first)) {
			throw new SimpleErrorException("Missing expected 'jmethod' SYMBOL. Got: " + first);
		}

		LispStruct cdr = methodForm.cdr();
		if (!(cdr instanceof ConsStruct)) {
			throw new SimpleErrorException("Missing 'jmethod' method name argument. Got: " + cdr);
		}

		final LispStruct second = FormAnalyzer.analyze(((ConsStruct) cdr).car(), environment);
		if (!(second instanceof final StringStruct methodName)) {
			throw new SimpleErrorException("Method name is not of expected type STRING. Got: " + second);
		}

		final String methodNameString = methodName.toJavaString();

		cdr = ((ConsStruct) cdr).cdr();
		if (!(cdr instanceof ConsStruct)) {
			throw new SimpleErrorException("Missing 'jmethod' class argument. Got: " + cdr);
		}

		// TODO: should do a FormAnalyze here??
		final LispStruct third = ((ConsStruct) cdr).car();
		if (!(third instanceof ConsStruct)) {
			throw new SimpleErrorException("Java class is not of expected type. Got: " + third);
		}

		final JavaClassStruct javaClassStruct = expandJavaClass((ConsStruct) third, environment);
		final Class<?> javaClass = javaClassStruct.getJavaClass();

		cdr = ((ConsStruct) cdr).cdr();
		if (!(cdr instanceof ListStruct)) {
			throw new SimpleErrorException("Missing 'jmethod' method arguments. Got: " + cdr);
		}

		final List<LispStruct> args = ((ListStruct) cdr).toJavaList();
		final Class<?>[] parameterTypes = new Class<?>[args.size()];
		for (int i = 0; i < args.size(); i++) {
			// TODO: should do a FormAnalyze here??
			final LispStruct currentArg = args.get(i);
			if (currentArg instanceof ConsStruct) {
				final JavaClassStruct methodParamClassStruct = expandJavaClass((ConsStruct) currentArg, environment);
				parameterTypes[i] = methodParamClassStruct.getJavaClass();
			} else {
				throw new TypeErrorException("Unexpected method parameter argument type to 'jmethod'. Got: " + currentArg);
			}
		}

		return JavaMethodStruct.toJavaMethod(methodNameString, javaClass, parameterTypes);
	}

	private static JavaClassStruct expandJavaClass(final ConsStruct classForm, final Environment environment) {
		final LispStruct first = classForm.car();
		if (!CommonLispSymbols.JCLASS.eql(first)) {
			throw new SimpleErrorException("Missing expected 'jclass' SYMBOL. Got: " + first);
		}

		LispStruct cdr = classForm.cdr();
		if (!(cdr instanceof ConsStruct)) {
			throw new SimpleErrorException("Missing 'jclass' class name argument. Got: " + cdr);
		}

		final LispStruct second = FormAnalyzer.analyze(((ConsStruct) cdr).car(), environment);
		if (!(second instanceof final StringStruct className)) {
			throw new SimpleErrorException("Class name is not of expected type STRING. Got: " + second);
		}

		final String classNameString = className.toJavaString();

		cdr = ((ConsStruct) cdr).cdr();
		if (!NILStruct.INSTANCE.eq(cdr)) {
			throw new SimpleErrorException("Unexpected arguments to 'jclass'. Got: " + cdr);
		}

		return JavaClassStruct.toJavaClass(classNameString);
	}
}
