package jcl.compiler.sa.analyzer.specialoperator.java;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.expanders.MacroFunctionExpander;
import jcl.compiler.sa.FormAnalyzer;
import jcl.compiler.struct.specialoperator.java.InvokeInstanceMethodCallStruct;
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
public class InvokeInterfaceExpander extends MacroFunctionExpander<InvokeInstanceMethodCallStruct> {

	public static final InvokeInterfaceExpander INSTANCE = new InvokeInterfaceExpander();

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.JINVOKE_INTERFACE;
	}

	@Override
	public InvokeInstanceMethodCallStruct expand(final ListStruct form, final Environment environment) {
		final Iterator<LispStruct> iterator = form.iterator();
		iterator.next(); // JINVOKE-INTERFACE SYMBOL

		final LispStruct method = iterator.next();
		if (!(method instanceof ConsStruct)) {
			throw new TypeErrorException("INVOKE-INTERFACE: Invalid Java Method. Got: " + method);
		}
		final JavaMethodStruct javaMethod = JavaMethodExpander.expandJavaMethod((ConsStruct) method, environment);

		final LispStruct object = FormAnalyzer.analyze(iterator.next(), environment);

		final List<LispStruct> args = new ArrayList<>();
		iterator.forEachRemaining(arg -> {
			final LispStruct analyzedArg = FormAnalyzer.analyze(arg, environment);
			args.add(analyzedArg);
		});

		return new InvokeInstanceMethodCallStruct(javaMethod, object, args);
	}
}
