package jcl.lists.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.NullStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.BooleanStructs;
import jcl.symbols.NILStruct;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class EndpFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public EndpFunction() {
		super("Returns true if list is the empty list. Returns false if list is a cons.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "LIST").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final LispStruct list = validator.validateType(lispStructs[0], functionName(), "List", ListType.INSTANCE, LispStruct.class);
		return BooleanStructs.toLispBoolean(NullStruct.INSTANCE.equals(list) || NILStruct.INSTANCE.equals(list));
	}

	@Override
	protected String functionName() {
		return "ENDP";
	}
}
