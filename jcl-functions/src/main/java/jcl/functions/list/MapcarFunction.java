package jcl.functions.list;

import java.util.List;

import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class MapcarFunction extends ConsMappingFunction {

	private static final String FUNCTION_NAME = "MAPCAR";
	private static final String FN_ARGUMENT = "FN";

	public MapcarFunction() {
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
		return map1(function, ListStruct.toLispList(lists), LIST_ACCUMULATE, true);
	}
}
