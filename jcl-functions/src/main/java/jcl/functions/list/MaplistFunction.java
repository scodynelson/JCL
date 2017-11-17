package jcl.functions.list;

import java.util.List;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class MaplistFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "MAPLIST";
	private static final String FN_ARGUMENT = "FN";

	public MaplistFunction() {
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
		return ListStruct.mapList(function, ListStruct.toLispList(lists));
	}
}
