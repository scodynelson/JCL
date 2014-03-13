package jcl.lambdalist;

import jcl.structs.LispStruct;
import jcl.lambdalist.variable.Aux;
import jcl.lambdalist.variable.Key;
import jcl.lambdalist.variable.Optional;
import jcl.lambdalist.variable.Rest;

import java.util.List;

public class OrdinaryLambdaList {

	private final List<LispStruct> vars;
	private final Optional optional;
	private final Rest rest;
	private final Key<LispStruct> key;
	private final Aux aux;

	public OrdinaryLambdaList(final List<LispStruct> vars, final Optional optional, final Rest rest, final Key<LispStruct> key,
							  final Aux aux) {
		this.vars = vars;
		this.optional = optional;
		this.rest = rest;
		this.key = key;
		this.aux = aux;
	}
}
