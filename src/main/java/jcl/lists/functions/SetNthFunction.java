package jcl.lists.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractSystemFunctionStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.IntegerType;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class SetNthFunction extends AbstractSystemFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public SetNthFunction() {
		super("Locates the nth element of list, where the car of the list is the ``zeroth'' element, and sets its value to the new-value provided.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "INDEX").build(),
				RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "LIST").build(),
				RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "VALUE").build()
		);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final IntegerStruct index
				= validator.validateType(lispStructs[0], functionName(), "Index", IntegerType.INSTANCE, IntegerStruct.class);
		final ListStruct list
				= validator.validateType(lispStructs[1], functionName(), "List", ListType.INSTANCE, ListStruct.class);
		final LispStruct value = lispStructs[2];

		final long indexValue = index.getBigInteger().longValue();
		list.setNth(indexValue, value);
		return value;
	}

	@Override
	protected String functionName() {
		return "SET-NTH";
	}
}
