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
public final class NreconcFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public NreconcFunction() {
		super("Reverses the order of elements in list (as if by nreverse). It then appends (as if by nconc) the tail to that reversed list and returns the result.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return Arrays.asList(
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "LIST").build(),
				RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "TAIL").build()
		);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final ListStruct list
				= validator.validateType(lispStructs[0], functionName(), "List", ListType.INSTANCE, ListStruct.class);
		final LispStruct tail = lispStructs[1];

		return list.nReconc(tail);
	}

	@Override
	protected String functionName() {
		return "NRECONC";
	}
}
