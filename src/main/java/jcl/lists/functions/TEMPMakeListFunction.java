package jcl.lists.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.KeyParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.BuiltInFunctionStruct;
import jcl.functions.FunctionParams;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.system.CommonLispSymbols;
import jcl.util.ClassUtils;
import org.springframework.stereotype.Component;

//@Component
public final class TEMPMakeListFunction extends BuiltInFunctionStruct<TEMPMakeListFunction.MakeListParams> {

	public TEMPMakeListFunction() {
		super("Returns a list of length given by size, each of the elements of which is initial-element.", "MAKE-LIST");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "SIZE").buildList();
	}

	@Override
	protected List<KeyParameter> getKeyBindings() {
		return KeyParameter.builder(GlobalPackageStruct.COMMON_LISP, "INITIAL-ELEMENT")
		                   .suppliedPBinding()
		                   .initForm(NILStruct.INSTANCE)
		                   .buildList();
	}

	@Override
	protected LispStruct internalApply(final MakeListParams params) {
		final IntegerStruct size = params.getSize();
		final LispStruct initialElement = params.getInitialElement();
		return ListStruct.makeList(size.getBigInteger().longValue(), initialElement);
	}

	@Override
	protected MakeListParams getParams(final OrdinaryLambdaList lambdaList) {
		final List<RequiredParameter> requiredBindings = lambdaList.getRequiredBindings();
		final RequiredParameter requiredParameter = requiredBindings.get(0);
		final IntegerStruct convertSize = ClassUtils.convert((Class<IntegerStruct>) requiredParameter.getInitFormClass(), requiredParameter.getInitForm());

		final List<KeyParameter> keyBindings = lambdaList.getKeyBindings();
		final KeyParameter keyParameter =
				keyBindings.stream()
				           .filter(keyBinding -> CommonLispSymbols.INITIAL_ELEMENT_KEYWORD.equals(keyBinding.getKeyName()))
				           .findAny()
				           .get();
		final LispStruct convertInitialElement = ClassUtils.convert(keyParameter.getInitFormClass(), keyParameter.getInitForm());

		return new MakeListParams(convertSize, convertInitialElement);
	}

	static class MakeListParams implements FunctionParams {

		private final IntegerStruct size;
		private final LispStruct initialElement;

		private MakeListParams(final IntegerStruct size, final LispStruct initialElement) {
			this.size = size;
			this.initialElement = initialElement;
		}

		private IntegerStruct getSize() {
			return size;
		}

		private LispStruct getInitialElement() {
			return initialElement;
		}
	}

}
