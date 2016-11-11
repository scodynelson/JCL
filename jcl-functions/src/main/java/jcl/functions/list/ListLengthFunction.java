package jcl.functions.list;

import java.math.BigInteger;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class ListLengthFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "LIST-LENGTH";
	private static final String LIST_ARGUMENT = "LIST";

	public ListLengthFunction() {
		super("Returns the length of list if list is a proper list. Returns nil if list is a circular list.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(LIST_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final ListStruct list = arguments.getRequiredArgument(LIST_ARGUMENT, ListStruct.class);
		final Long listLength = list.listLength();
		if (listLength == null) {
			return NILStruct.INSTANCE;
		}
		return LispStructFactory.toInteger(BigInteger.valueOf(listLength));
	}
}
