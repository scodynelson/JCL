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
public final class LdiffFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public LdiffFunction() {
		super("If object is the same as some tail of list, returns a fresh list of the elements of list that precede object in the list structure of list; otherwise, it returns a copy of list.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "LIST").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "OBJECT").build()
		);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final ListStruct list = validator.validateType(lispStructs[0], functionName(), "List", ListType.INSTANCE, ListStruct.class);
		final LispStruct object = lispStructs[1];
		return list.ldiff(object);
	}

	@Override
	protected String functionName() {
		return "LDIFF";
	}
}
