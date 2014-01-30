package java.util;

public class ResizableHashMap<K, V> extends HashMap<K, V> {

	private static final long serialVersionUID = -2266420564872715843L;

	private final int rehashSize;

	public ResizableHashMap() {
		rehashSize = DEFAULT_INITIAL_CAPACITY;
	}

	public ResizableHashMap(final int initialCapacity) {
		super(initialCapacity);
		rehashSize = DEFAULT_INITIAL_CAPACITY;
	}

	public ResizableHashMap(final int initialCapacity, final float loadFactor) {
		super(initialCapacity, loadFactor);
		rehashSize = DEFAULT_INITIAL_CAPACITY;
	}

	public ResizableHashMap(final int initialCapacity, final float loadFactor, final int rehashSize) {
		super(initialCapacity, loadFactor);
		this.rehashSize = rehashSize;
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	@Override
	void resize(final int newCapacity) {
		final Entry[] oldTable = table;
		final int oldCapacity = oldTable.length;
		if (oldCapacity == MAXIMUM_CAPACITY) {
			threshold = Integer.MAX_VALUE;
			return;
		}

		final int realNewCapacity = threshold + rehashSize;

		final Entry[] newTable = new Entry[realNewCapacity];
		transfer(newTable, initHashSeedAsNeeded(realNewCapacity));
		table = newTable;
		threshold = Math.round(Math.min(realNewCapacity * loadFactor, MAXIMUM_CAPACITY + 1));
	}
}
