package jcl.lists.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class GetPropertiesFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public GetPropertiesFunction() {
		super("Used to look up any of several property list entries all at once..");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "PLIST").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "INDICATOR-LIST").build()
		);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final ListStruct plist =
				validator.validateType(lispStructs[0], functionName(), "Property List", ListType.INSTANCE, ListStruct.class);
		final ListStruct indicatorList =
				validator.validateType(lispStructs[1], functionName(), "Indicator List", ListType.INSTANCE, ListStruct.class);

		return plist.getProperties(indicatorList);
	}

	@Override
	protected String functionName() {
		return "GET-PROPERTIES";
	}
}
