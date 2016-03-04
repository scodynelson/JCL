package jcl.lists.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
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
public final class NthcdrFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public NthcdrFunction() {
		super("Returns the tail of list that would be obtained by calling cdr n times in succession.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "N").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "LIST").build()
		);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final IntegerStruct nVal = validator.validateType(lispStructs[0], functionName(), "N", IntegerType.INSTANCE, IntegerStruct.class);
		final ListStruct list = validator.validateType(lispStructs[1], functionName(), "List", ListType.INSTANCE, ListStruct.class);

		final long nLong = nVal.getBigInteger().longValue();
		return list.nthCdr(nLong);
	}

	@Override
	protected String functionName() {
		return "NTHCDR";
	}
}
