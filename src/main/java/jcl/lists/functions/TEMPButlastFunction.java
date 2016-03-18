package jcl.lists.functions;

import java.util.List;

import jcl.LispStruct;
import jcl.functions.BuiltInFunctionStruct;
import jcl.functions.parameterdsl.FunctionParameters;
import jcl.functions.parameterdsl.Parameters;
import jcl.lists.ListStruct;
import jcl.numbers.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class TEMPButlastFunction extends BuiltInFunctionStruct {

	public TEMPButlastFunction() {
		super("Returns a copy of list from which the last n conses have been omitted.", "TEMP-BUTLAST");
	}

	@Override
	public LispStruct apply(final FunctionParameters params) {
		final ListStruct list = (ListStruct) params.getRequiredParameters().get("list");
		final IntegerStruct n = (IntegerStruct) params.getOptionalParameters().get("n");
		return list.butLast(n.getBigInteger().longValue());
	}

	@Override
	protected FunctionParameters getParams(final List<LispStruct> lispStructs) {
		return Parameters.forFunction("TEMP-BUTLAST")
		                 .requiredParameter("list").as(ListStruct.class)
		                 .optionalParameter("n").withInitialValue(IntegerStruct.ONE).as(IntegerStruct.class)
		                 .build(lispStructs);
	}
}
