package jcl.lists.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.OrdinaryLambdaList;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.BuiltInFunctionStruct;
import jcl.functions.FunctionParams;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.util.ClassUtils;
import org.springframework.stereotype.Component;

//@Component
public final class TEMPButlastFunction extends BuiltInFunctionStruct<TEMPButlastFunction.ButLastParams> {

	public TEMPButlastFunction() {
		super("Returns a copy of list from which the last n conses have been omitted.", "BUTLAST");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "LIST").buildList();
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "N")
		                        .initForm(IntegerStruct.ONE)
		                        .suppliedPBinding()
		                        .buildList();
	}

	@Override
	protected LispStruct internalApply(final ButLastParams params) {
		final ListStruct list = params.getList();
		final IntegerStruct n = params.getN();
		return list.butLast(n.getBigInteger().longValue());
	}

	@Override
	protected ButLastParams getParams(final OrdinaryLambdaList lambdaList) {
		final List<RequiredParameter> requiredBindings = lambdaList.getRequiredBindings();
		final RequiredParameter requiredParameter = requiredBindings.get(0);
		final ListStruct convertList = ClassUtils.convert((Class<ListStruct>) requiredParameter.getInitFormClass(), requiredParameter.getInitForm());

		final List<OptionalParameter> optionalBindings = lambdaList.getOptionalBindings();
		final OptionalParameter optionalParameter = optionalBindings.get(0);
		final IntegerStruct convertN = ClassUtils.convert((Class<IntegerStruct>) optionalParameter.getInitFormClass(), optionalParameter.getInitForm());

		return new ButLastParams(convertList, convertN);
	}

	static class ButLastParams implements FunctionParams {

		private final ListStruct list;
		private final IntegerStruct n;

		private ButLastParams(final ListStruct list, final IntegerStruct n) {
			this.list = list;
			this.n = n;
		}

		private ListStruct getList() {
			return list;
		}

		private IntegerStruct getN() {
			return n;
		}
	}

}
