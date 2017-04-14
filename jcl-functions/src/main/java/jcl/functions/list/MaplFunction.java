package jcl.functions.list;

import java.util.List;

import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class MaplFunction extends ConsMappingFunction {

	private static final String FUNCTION_NAME = "MAPL";
	private static final String FN_ARGUMENT = "FN";

	public MaplFunction() {
		super("",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(FN_ARGUMENT)
		                .restParameter()
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final FunctionStruct function = arguments.getRequiredArgument(FN_ARGUMENT, FunctionStruct.class);
		final List<LispStruct> lists = arguments.getRestArgument();
		return map1(function, LispStructFactory.toProperList(lists), null, false);
	}
}
