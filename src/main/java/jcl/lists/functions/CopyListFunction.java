package jcl.lists.functions;

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
public final class CopyListFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public CopyListFunction() {
		super("Returns a copy of list.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "LIST").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final ListStruct list = validator.validateType(lispStructs[0], functionName(), "List", ListType.INSTANCE, ListStruct.class);
		return list.copyList();
	}

	@Override
	protected String functionName() {
		return "COPY-LIST";
	}
}
