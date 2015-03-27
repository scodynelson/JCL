package jcl.compiler.real.functions;

import java.util.Collections;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.real.environment.allocation.ParameterAllocation;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.sa.analyzer.LambdaExpander;
import jcl.compiler.real.sa.analyzer.expander.MacroFunctionExpander;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.FunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class CompileFunction extends FunctionStruct {

	public static final CompileFunction INSTANCE = new CompileFunction();

	public static final SymbolStruct<?> COMPILE = new SymbolStruct<>("COMPILE", GlobalPackageStruct.COMMON_LISP, null, INSTANCE);

	private static final long serialVersionUID = 5339244651961527815L;

	private static String definitionSuppliedPName;

	@Autowired
	private CompileForm compileForm;

	@Autowired
	private LambdaExpander lambdaExpander;

	public CompileFunction() {
		super("Produces a compiled function from definition.", getInitLambdaListBindings());
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> nameArgSymbol = new SymbolStruct<>("NAME", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation nameArgAllocation = new ParameterAllocation(0);
		final RequiredBinding requiredBinding = new RequiredBinding(nameArgSymbol, nameArgAllocation);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> definitionArgSymbol = new SymbolStruct<>("DEFINITION", GlobalPackageStruct.COMMON_LISP);
		final ParameterAllocation definitionArgAllocation = new ParameterAllocation(1);

		definitionSuppliedPName = "DEFINITION-P-" + System.nanoTime();
		final SymbolStruct<?> definitionSuppliedP = new SymbolStruct<>(definitionSuppliedPName, GlobalPackageStruct.SYSTEM);

		final ParameterAllocation suppliedPAllocation = new ParameterAllocation(2);
		final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(definitionSuppliedP, suppliedPAllocation);

		final OptionalBinding optionalBinding = new OptionalBinding(definitionArgSymbol, definitionArgAllocation, NullStruct.INSTANCE, suppliedPBinding);
		final List<OptionalBinding> optionalBindings = Collections.singletonList(optionalBinding);

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		final LispStruct name = lispStructs[0];

		final SymbolStruct<?> suppliedPSymbol = GlobalPackageStruct.SYSTEM.findSymbol(definitionSuppliedPName).getSymbol();
		LispStruct suppliedPValue = suppliedPSymbol.getLexicalValue();
		if (suppliedPValue instanceof ValuesStruct) {
			final ValuesStruct values = (ValuesStruct) suppliedPValue;
			suppliedPValue = values.getPrimaryValue();
		}

		if (!suppliedPValue.equals(NullStruct.INSTANCE) && !suppliedPValue.equals(NILStruct.INSTANCE)) {
			final LispStruct uncompiledDefinition = lispStructs[1];

			final CompileResult compiledDefinition = compileForm.compile(uncompiledDefinition);
			final FunctionStruct function = compiledDefinition.getFunction();

			if (name instanceof SymbolStruct) {
				final SymbolStruct<?> nameSymbol = (SymbolStruct<?>) name;
				nameSymbol.setFunction(function);
			} else if (!name.equals(NullStruct.INSTANCE) && !name.equals(NILStruct.INSTANCE)) {
				throw new ErrorException("The value " + name + " is not an acceptable function name.");
			}

			return new ValuesStruct(function, compiledDefinition.isCompiledWithWarnings(), compiledDefinition.isFailedToCompile());
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
