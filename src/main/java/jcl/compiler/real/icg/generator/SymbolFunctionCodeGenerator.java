package jcl.compiler.real.icg.generator;

import jcl.compiler.real.icg.IntermediateCodeGenerator;
import jcl.compiler.real.icg.JavaClassBuilder;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.SymbolStruct;
import org.objectweb.asm.Label;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SymbolFunctionCodeGenerator implements CodeGenerator<SymbolStruct<?>> {

	private static final Logger LOGGER = LoggerFactory.getLogger(SymbolFunctionCodeGenerator.class);

	@Autowired
	private SpecialVariableCodeGenerator specialVariableCodeGenerator;

	@Override
	public void generate(final SymbolStruct<?> input, final IntermediateCodeGenerator codeGenerator, final JavaClassBuilder classBuilder) {
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
				classBuilder.getEmitter().visitMethodLabel(label);
				classBuilder.getEmitter().emitGetstatic("lisp/extensions/type/CommonLispFunctions", "StdFunctions", "Llisp/extensions/type/CommonLispFunctions;");
				// +1 -> StdFns
				// TODO: the following 5 lines...
//				if (input.getFunction() instanceof MacroFunctionExpander) {
//					codeGenerator.emitter.emitGetfield("lisp/extensions/type/CommonLispFunctions", fnFieldName, "Llisp/common/type/MacroFunction;");
//				} else {
//					codeGenerator.emitter.emitGetfield("lisp/extensions/type/CommonLispFunctions", fnFieldName, canonicalName);//"Llisp/common/type/Function;");
//				}
				// +1 -> fn
			} else {
				final Label label = new Label();
				classBuilder.getEmitter().visitMethodLabel(label);
				specialVariableCodeGenerator.generate(input, codeGenerator, classBuilder);
				// invoke symbol.getFunction()
				classBuilder.getEmitter().emitInvokeinterface("lisp/common/type/Symbol", "getFunction", "()", "Llisp/common/type/Function;", true);
				// if the symbol has defined less than 12 params, we can say that it takes that number of args
			}
		} else {
			final Label label = new Label();
			classBuilder.getEmitter().visitMethodLabel(label);
			specialVariableCodeGenerator.generate(input, codeGenerator, classBuilder);
			// invoke symbol.getFunction()
			classBuilder.getEmitter().emitInvokeinterface("lisp/common/type/Symbol", "getFunction", "()", "Llisp/common/type/Function;", true);
			// if the symbol has defined less than 12 params, we can say that it takes that number of args
		}
	}
}