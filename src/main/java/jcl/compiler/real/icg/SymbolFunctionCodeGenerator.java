package jcl.compiler.real.icg;

import jcl.compiler.old.expander.MacroFunctionExpander;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.structs.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SymbolFunctionCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(SymbolFunctionCodeGenerator.class);

	public static final SymbolFunctionCodeGenerator INSTANCE = new SymbolFunctionCodeGenerator();

	@Override
	public void generate(final SymbolStruct<?> input, final IntermediateCodeGenerator codeGenerator) {
		// there are multiple ways to handle this
		// we add an optimization for calling a CL function
		// it becomes a static field reference instead of a runtime symbol lookup
		// +0 ->
		if (input.getSymbolPackage().equals(GlobalPackageStruct.COMMON_LISP)) {
			String fnFieldName = "FUNCTION NAME"; // TODO: CommonLispFunctions.getFieldName(sym.getName().toString());
			// get the type of the field as well...
			if (fnFieldName != null) {
//				CommonLispFunctions clf = CommonLispFunctions.StdFunctions; // TODO
				String canonicalName = null;
				try {
					canonicalName = "CANONICAL NAME"; // TODO: clf.getClass().getDeclaredField(fnFieldName).getType().getCanonicalName().toString();
					canonicalName = canonicalName.replace('.', '/');
					canonicalName = 'L' + canonicalName + ';';
				} catch (final Exception ex) {
					LOGGER.warn(ex.getMessage(), ex);
				}
				final String[] strs = fnFieldName.split("\\.");
				if (strs.length > 0) {
					fnFieldName = strs[strs.length - 1];
				}
				final Label label = new Label();
				codeGenerator.emitter.visitMethodLabel(label);
				codeGenerator.emitter.emitGetstatic("lisp/extensions/type/CommonLispFunctions", "StdFunctions", "Llisp/extensions/type/CommonLispFunctions;");
				// +1 -> StdFns
				if (input.getFunction() instanceof MacroFunctionExpander) {
					codeGenerator.emitter.emitGetfield("lisp/extensions/type/CommonLispFunctions", fnFieldName, "Llisp/common/type/MacroFunction;");
				} else {
					codeGenerator.emitter.emitGetfield("lisp/extensions/type/CommonLispFunctions", fnFieldName, canonicalName);//"Llisp/common/type/Function;");
				}
				// +1 -> fn
			} else {
				final Label label = new Label();
				codeGenerator.emitter.visitMethodLabel(label);
				codeGenerator.genCodeSpecialVariable(input);
				// invoke symbol.getFunction()
				codeGenerator.emitter.emitInvokeinterface("lisp/common/type/Symbol", "getFunction", "()", "Llisp/common/type/Function;", true);
				// if the symbol has defined less than 12 params, we can say that it takes that number of args
			}
		} else {
			final Label label = new Label();
			codeGenerator.emitter.visitMethodLabel(label);
			codeGenerator.genCodeSpecialVariable(input);
			// invoke symbol.getFunction()
			codeGenerator.emitter.emitInvokeinterface("lisp/common/type/Symbol", "getFunction", "()", "Llisp/common/type/Function;", true);
			// if the symbol has defined less than 12 params, we can say that it takes that number of args
		}
	}
}
