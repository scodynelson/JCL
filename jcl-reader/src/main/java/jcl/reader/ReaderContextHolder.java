package jcl.reader;

import lombok.experimental.UtilityClass;

@UtilityClass
public final class ReaderContextHolder {

	private static final ThreadLocal<ReaderContext> contextHolder = new ThreadLocal<>();

	public static void clearContext() {
		contextHolder.remove();
	}

	public static ReaderContext getContext() {
		ReaderContext ctx = contextHolder.get();

		if (ctx == null) {
			ctx = createEmptyContext();
			contextHolder.set(ctx);
		}

		return ctx;
	}

	public static void setContext(final ReaderContext context) {
		assert context != null : "Only non-null ReaderContext instances are permitted";
		contextHolder.set(context);
	}

	private static ReaderContext createEmptyContext() {
		return new ReaderContext();
	}
}
