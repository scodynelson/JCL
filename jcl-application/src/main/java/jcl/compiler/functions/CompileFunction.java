package jcl.compiler.functions;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.expander.MacroFunctionExpanderInter;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class CompileFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "COMPILE";
	private static final String NAME_ARGUMENT = "NAME";
	private static final String DEFINITION_ARGUMENT = "DEFINITION";

	@Autowired
	private CompileForm compileForm;

	@Autowired
	private Printer printer;

	public CompileFunction() {
		super("Produces a compiled function from definition.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(NAME_ARGUMENT)
		                .optionalParameter(DEFINITION_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {

		final LispStruct name = arguments.getRequiredArgument(NAME_ARGUMENT);
		LispStruct uncompiledDefinition = null;
		if (arguments.hasOptionalArgument(DEFINITION_ARGUMENT)) {
			uncompiledDefinition = arguments.getOptionalArgument(DEFINITION_ARGUMENT);
		}

		return compile(name, uncompiledDefinition);
	}

	private LispStruct compile(final LispStruct name, final LispStruct uncompiledDefinition) {

		if (uncompiledDefinition != null) {
			CompileResult compiledDefinition = null;

			final FunctionStruct function;
			if (uncompiledDefinition instanceof FunctionStruct) {
				function = (FunctionStruct) uncompiledDefinition;
			} else {
				compiledDefinition = compileForm.compile(uncompiledDefinition);
				final FunctionStruct compiledDefinitionFunction = compiledDefinition.getFunction();
				final LispStruct compiledDefinitionResult = compiledDefinitionFunction.apply();

				if (!(compiledDefinitionResult instanceof FunctionStruct)) {
					final String printedObject = printer.print(uncompiledDefinition);
					throw new ProgramErrorException("Error compiling anonymous function : " + printedObject + " is not a valid lambda expression.");
				}
				function = (FunctionStruct) compiledDefinitionResult;
			}

			if (name instanceof SymbolStruct) {
				final SymbolStruct nameSymbol = (SymbolStruct) name;
				nameSymbol.setFunction(function);
			} else if (!NILStruct.INSTANCE.equals(name)) {
				throw new ErrorException("The value " + name + " is not an acceptable function name.");
			}

			if (compiledDefinition == null) {
				return new ValuesStruct(function, NILStruct.INSTANCE, NILStruct.INSTANCE);
			} else {
				return new ValuesStruct(function, compiledDefinition.isCompiledWithWarnings(), compiledDefinition.isFailedToCompile());
			}
		}

		if (!(name instanceof SymbolStruct)) {
			throw new ErrorException("The value " + name + " is not an acceptable function name.");
		}
		final SymbolStruct nameSymbol = (SymbolStruct) name;

		final MacroFunctionExpanderInter macroFunction = nameSymbol.getMacroFunctionExpander();
		if (macroFunction != null) {
			return new ValuesStruct(macroFunction, NILStruct.INSTANCE, NILStruct.INSTANCE);
		}

		final boolean hasFunction = nameSymbol.hasFunction();
		if (hasFunction) {
			final FunctionStruct function = nameSymbol.getFunction();
			return new ValuesStruct(function, NILStruct.INSTANCE, NILStruct.INSTANCE);
		}

		throw new ErrorException("No definition found for " + nameSymbol);
	}
}
