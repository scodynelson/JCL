package jcl.compiler.real.environment;

import jcl.LispStruct;
import jcl.LispType;
import jcl.symbols.SymbolStruct;

import java.util.List;

public class Environment implements LispStruct {

	public static final Environment FREE = null;
	public static final Environment NULL = null;

	private Marker marker;
	private LoadTimeValue loadTimeValue;
	private Environment parent;
	private List<Binding> bindings;
	private SymbolTable symbolTable;
	private Closure environmentClosure;

	// TODO: load-time-value ???
	public Environment(final Marker marker, final LoadTimeValue loadTimeValue, final Environment parent, final List<Binding> bindings,
					   final SymbolTable symbolTable, final Closure environmentClosure) {
		this.marker = marker;
		this.parent = parent;
		this.loadTimeValue = loadTimeValue;
		this.bindings = bindings;
		this.symbolTable = symbolTable;
		this.environmentClosure = environmentClosure;
	}

	public Marker getMarker() {
		return marker;
	}

	public LoadTimeValue getLoadTimeValue() {
		return loadTimeValue;
	}

	public Environment getParent() {
		return parent;
	}

	public List<Binding> getBindings() {
		return bindings;
	}

	public Binding getBinding(final SymbolStruct symbolStruct) {
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

	public void setMarker(final Marker marker) {
		this.marker = marker;
	}

	public void setLoadTimeValue(final LoadTimeValue loadTimeValue) {
		this.loadTimeValue = loadTimeValue;
	}

	public void setParent(final Environment parent) {
		this.parent = parent;
	}

	public void setBindings(final List<Binding> bindings) {
		this.bindings = bindings;
	}

	public void setSymbolTable(final SymbolTable symbolTable) {
		this.symbolTable = symbolTable;
	}

	public void setEnvironmentClosure(final Closure environmentClosure) {
		this.environmentClosure = environmentClosure;
	}

	@Override
	public LispType getType() {
		return null;
	}
}
