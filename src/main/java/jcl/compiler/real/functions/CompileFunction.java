package jcl.compiler.real.functions;

import java.util.Collections;
import java.util.List;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.compiler.real.environment.binding.lambdalist.AuxBinding;
import jcl.compiler.real.environment.binding.lambdalist.KeyBinding;
import jcl.compiler.real.environment.binding.lambdalist.OptionalBinding;
import jcl.compiler.real.environment.binding.lambdalist.OrdinaryLambdaListBindings;
import jcl.compiler.real.environment.binding.lambdalist.RequiredBinding;
import jcl.compiler.real.environment.binding.lambdalist.RestBinding;
import jcl.compiler.real.environment.binding.lambdalist.SuppliedPBinding;
import jcl.compiler.real.struct.ValuesStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.FunctionStruct;
import jcl.functions.expanders.MacroFunctionExpander;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class CompileFunction extends FunctionStruct {

	public static final SymbolStruct<?> COMPILE = new SymbolStruct<>("COMPILE", GlobalPackageStruct.COMMON_LISP);

	private static final long serialVersionUID = 5339244651961527815L;

	@Autowired
	private CompileForm compileForm;

	private CompileFunction() {
		super("Produces a compiled function from definition.", getInitLambdaListBindings());
	}

	@PostConstruct
	private void init() {
		COMPILE.setFunction(this);
	}

	private static OrdinaryLambdaListBindings getInitLambdaListBindings() {

		final SymbolStruct<?> nameArgSymbol = new SymbolStruct<>("NAME", GlobalPackageStruct.COMMON_LISP);
		final RequiredBinding requiredBinding = new RequiredBinding(nameArgSymbol);
		final List<RequiredBinding> requiredBindings = Collections.singletonList(requiredBinding);

		final SymbolStruct<?> definitionArgSymbol = new SymbolStruct<>("DEFINITION", GlobalPackageStruct.COMMON_LISP);

		final SymbolStruct<?> definitionSuppliedP = new SymbolStruct<>("DEFINITION-P-" + System.nanoTime(), GlobalPackageStruct.SYSTEM);
		final SuppliedPBinding suppliedPBinding = new SuppliedPBinding(definitionSuppliedP);

		final OptionalBinding optionalBinding = new OptionalBinding(definitionArgSymbol, NullStruct.INSTANCE, suppliedPBinding);
		final List<OptionalBinding> optionalBindings = Collections.singletonList(optionalBinding);

		final RestBinding restBinding = null;

		final List<KeyBinding> keyBindings = Collections.emptyList();
		final boolean allowOtherKeys = false;
		final List<AuxBinding> auxBindings = Collections.emptyList();

		return new OrdinaryLambdaListBindings(requiredBindings, optionalBindings, restBinding, keyBindings, auxBindings, allowOtherKeys);
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
				function = compiledDefinition.getFunction();
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
