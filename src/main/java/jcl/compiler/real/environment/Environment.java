package jcl.compiler.real.environment;

import jcl.LispStruct;
import jcl.LispType;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayList;
import java.util.List;

public class Environment implements LispStruct {

	public static final Environment FREE = new Environment(null, Marker.LAMBDA, 0);
	public static final Environment NULL = new Environment(null, Marker.LAMBDA, 0);

	private final Environment parent;
	private final Marker marker;

	private final List<LoadTimeValue> loadTimeValues = new ArrayList<>();
	private final List<Binding> bindings = new ArrayList<>();
	private final SymbolTable symbolTable = new SymbolTable();
	private final Closure environmentClosure;

	// TODO: load-time-value ???
	public Environment(final Environment parent, final Marker marker, final int closureDepth) {
		this.parent = parent;
		this.marker = marker;
		environmentClosure = new Closure(closureDepth);
	}

	public Environment getParent() {
		return parent;
	}

	public Marker getMarker() {
		return marker;
	}

	public List<LoadTimeValue> getLoadTimeValues() {
		return loadTimeValues;
	}

	public List<Binding> getBindings() {
		return bindings;
	}

	public Binding getBinding(final SymbolStruct<?> symbolStruct) {
		for (final Binding binding : bindings) {
			if (binding.getSymbolStruct().equals(symbolStruct)) {
				return binding;
			}
		}
		return null;
	}

	public SymbolTable getSymbolTable() {
		return symbolTable;
	}

	public Closure getEnvironmentClosure() {
		return environmentClosure;
	}

	@Override
	public LispType getType() {
		return null;
	}

	@Override
	public String printStruct() {
		return toString();
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
