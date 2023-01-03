package jcl.compiler.sa.analyzer.specialoperator.java;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.java.InvokeStaticMethodCallStruct;
import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.java.JavaMethodStruct;
import jcl.lang.statics.CommonLispSymbols;
import lombok.AccessLevel;
import lombok.NoArgsConstructor;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class InvokeStaticExpander extends MacroFunctionExpander<InvokeStaticMethodCallStruct> {

	public static final InvokeStaticExpander INSTANCE = new InvokeStaticExpander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.JINVOKE_STATIC;
	}

	@Override
	public InvokeStaticMethodCallStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // JINVOKE-STATIC SYMBOL

		final LispStruct method = iterator.next();
		if (!(method instanceof ConsStruct)) {
			throw new TypeErrorException("INVOKE-STATIC: Invalid Java Method. Got: " + method);
		}
		final JavaMethodStruct javaMethod = JavaMethodExpander.expandJavaMethod((ConsStruct) method, environment);

		final List<LispStruct> args = new ArrayList<>();
		iterator.forEachRemaining(arg -> {
			final LispStruct analyzedArg = FormAnalyzer.analyze(arg, environment);
			args.add(analyzedArg);
		});

		return new InvokeStaticMethodCallStruct(javaMethod, args);
	}
}
