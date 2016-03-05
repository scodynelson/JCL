package jcl.lists.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractSystemFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class SetGetfFunction extends AbstractSystemFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public SetGetfFunction() {
		super("Finds a property on the property list whose property indicator is identical to indicator, and sets its corresponding property value with the new-value provided.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "PLIST").build(),
				RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "INDICATOR").build(),
				RequiredParameter.builder(GlobalPackageStruct.SYSTEM, "VALUE").build()
		);
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.SYSTEM, "DEFAULT")
		                        .suppliedPBinding()
		                        .buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final ListStruct plist =
				validator.validateType(lispStructs[0], functionName(), "Property List", ListType.INSTANCE, ListStruct.class);
		final LispStruct indicator = lispStructs[1];
		final LispStruct value = lispStructs[2];

		plist.setProperty(indicator, value);
		return value;
	}

	@Override
	protected String functionName() {
		return "SET-GETF";
	}
}
