package jcl.lists.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.IntegerType;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class NbutlastFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public NbutlastFunction() {
		super("Returns a list from which the last n conses have been omitted. The provided list may be modified.");
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
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final ListStruct list =
				validator.validateType(lispStructs[0], functionName(), "List", ListType.INSTANCE, ListStruct.class);

		final long nVal;
		if (lispStructs.length > 1) {
			final IntegerStruct n =
					validator.validateType(lispStructs[1], functionName(), "N", IntegerType.INSTANCE, IntegerStruct.class);
			nVal = n.getBigInteger().longValue();
		} else {
			nVal = 1;
		}

		return list.nButLast(nVal);
	}

	@Override
	protected String functionName() {
		return "NBUTLAST";
	}
}
