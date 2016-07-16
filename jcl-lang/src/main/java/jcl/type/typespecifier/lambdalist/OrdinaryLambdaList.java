package jcl.type.typespecifier.lambdalist;

import java.util.List;

import jcl.lang.LispStruct;
import jcl.type.typespecifier.lambdalist.variable.Aux;
import jcl.type.typespecifier.lambdalist.variable.Key;
import jcl.type.typespecifier.lambdalist.variable.Optional;
import jcl.type.typespecifier.lambdalist.variable.Rest;

public class OrdinaryLambdaList {

	private final List<LispStruct> vars;

	private final Optional<?> optional;

	private final Rest<?> rest;

	private final Key<LispStruct> key;

	private final Aux<?> aux;

	public OrdinaryLambdaList(final List<LispStruct> vars, final Optional<?> optional, final Rest<?> rest, final Key<LispStruct> key,
	                          final Aux<?> aux) {
		this.vars = vars;
		this.optional = optional;
		this.rest = rest;
		this.key = key;
		this.aux = aux;
	}
}
