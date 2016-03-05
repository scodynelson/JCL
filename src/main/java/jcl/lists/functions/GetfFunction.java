package jcl.lists.functions;

import java.util.Arrays;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.OptionalParameter;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class GetfFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	public GetfFunction() {
		super("Finds a property on the property list whose property indicator is identical to indicator, and returns its corresponding property value.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "PLIST").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "INDICATOR").build()
		);
	}

	@Override
	protected List<OptionalParameter> getOptionalBindings() {
		return OptionalParameter.builder(GlobalPackageStruct.COMMON_LISP, "DEFAULT")
		                        .suppliedPBinding()
		                        .buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final ListStruct plist =
				validator.validateType(lispStructs[0], functionName(), "Property List", ListType.INSTANCE, ListStruct.class);
		final LispStruct indicator = lispStructs[1];

		final LispStruct defaultValue;
		if (lispStructs.length > 2) {
			defaultValue = lispStructs[2];
		} else {
			defaultValue = NILStruct.INSTANCE;
		}

		return plist.getProperty(indicator, defaultValue);
	}

	@Override
	protected String functionName() {
		return "GETF";
	}
}
