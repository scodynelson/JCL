package jcl.lists.functions;

import java.math.BigInteger;
import java.util.List;

import jcl.LispStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.types.ListType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public final class ListLengthFunction extends AbstractCommonLispFunctionStruct {

	@Autowired
	private TypeValidator validator;

	public ListLengthFunction() {
		super("Returns the length of list if list is a proper list. Returns nil if list is a circular list.");
	}

	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "LIST").buildList();
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final ListStruct list = validator.validateType(lispStructs[0], functionName(), "List", ListType.INSTANCE, ListStruct.class);
		final Long listLength = list.listLength();
		if (listLength == null) {
			return NILStruct.INSTANCE;
		}
		return new IntegerStruct(BigInteger.valueOf(listLength));
	}

	@Override
	protected String functionName() {
		return "LIST-LENGTH";
	}
}
