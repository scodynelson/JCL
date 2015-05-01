package jcl.compiler.real.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.FunctionStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class CompileFunction extends FunctionStruct {

	public static final SymbolStruct<?> COMPILE = GlobalPackageStruct.COMMON_LISP.intern("COMPILE").getSymbol();

	private static final long serialVersionUID = 5339244651961527815L;

	@Autowired
	private CompileForm compileForm;

	@Autowired
	private Printer printer;

	private CompileFunction() {
		super("Produces a compiled function from definition.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		COMPILE.setFunction(this);
		GlobalPackageStruct.COMMON_LISP.export(COMPILE);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> nameArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("NAME").getSymbol();
		final RequiredBinding requiredBinding = new RequiredBinding(nameArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> definitionArgSymbol = GlobalPackageStruct.COMMON_LISP.intern("DEFINITION").getSymbol();

		final SymbolStruct<?> definitionSuppliedP = GlobalPackageStruct.COMMON_LISP.intern("DEFINITION-P-" + System.nanoTime()).getSymbol();
		final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(definitionSuppliedP);

		final OptionalBinding optionalBinding = new OptionalBinding(definitionArgSymbol, NullStruct.INSTANCE, suppliedPBinding);
		final List<OptionalBinding> optionalBindings = Collections.singletonList(optionalBinding);

		return new OrdinaryLambdaListBindings.Builder().requiredBindings(requiredBindings)
		                                               .optionalBindings(optionalBindings)
		                                               .build();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		getFunctionBindings(lispStructs);

		final LispStruct name = lispStructs[0];
		LispStruct uncompiledDefinition = null;
		if (lispStructs.length == 2) {
			uncompiledDefinition = lispStructs[1];
		}

		return compile(name, uncompiledDefinition);
	}

	public LispStruct compile(final LispStruct name, final LispStruct uncompiledDefinition) {

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
				final SymbolStruct<?> nameSymbol = (SymbolStruct<?>) name;
				nameSymbol.setFunction(function);
			} else if (!name.equals(NullStruct.INSTANCE) && !name.equals(NILStruct.INSTANCE)) {
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
		final SymbolStruct<?> nameSymbol = (SymbolStruct<?>) name;

		final MacroFunctionExpander<?> macroFunction = nameSymbol.getMacroFunctionExpander();
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
