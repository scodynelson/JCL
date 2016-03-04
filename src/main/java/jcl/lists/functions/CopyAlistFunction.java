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
public final class CopyAlistFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public CopyAlistFunction() {
		super("Returns a copy of alist.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "ALIST").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final ListStruct list = validator.validateType(lispStructs[0], functionName(), "Association-List", ListType.INSTANCE, ListStruct.class);
		return list.copyAlist();
	}

	@Override
	protected String functionName() {
		return "COPY-ALIST";
	}
}
