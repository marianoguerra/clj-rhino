package org.marianoguerra.rhino;

import org.mozilla.javascript.*;

public class TimedContextFactory extends ContextFactory {
	// Custom Context to store execution time.
	private static class TimedContext extends Context {
		long startTime;

		public TimedContext(ContextFactory factory) {
			super(factory);
		}
	}

	private long timeoutMillis;
	private int ticksInterval;

	public TimedContextFactory(long timeoutMillis) {
		this(timeoutMillis, 10000);
	}

	public TimedContextFactory(long timeoutMillis, int ticksInterval) {
		this.timeoutMillis = timeoutMillis;
		this.ticksInterval = ticksInterval;
	}

	// Override makeContext()
	protected Context makeContext() {
		TimedContext cx = new TimedContext(this);
		// Make Rhino runtime to call observeInstructionCount
		// each ticksInterval bytecode instructions
		cx.setInstructionObserverThreshold(this.ticksInterval);
		return cx;
	}

	// Override observeInstructionCount(Context, int)
	protected void observeInstructionCount(Context cx,
			int instructionCount) {
		TimedContext mcx = (TimedContext)cx;
		long currentTime = System.currentTimeMillis();

		if (currentTime - mcx.startTime > this.timeoutMillis) {
			// More then timeoutMillis from Context creation time:
			// it is time to stop the script.
			// Throw Error instance to ensure that script will never
			// get control back through catch or finally.
			throw new Error();
		}
	}

	// Override doTopCall(Callable, Context, Scriptable, Scriptable, Object[])
	protected Object doTopCall(Callable callable,
			Context cx, Scriptable scope,
			Scriptable thisObj, Object[] args) {

		TimedContext mcx = (TimedContext)cx;
		mcx.startTime = System.currentTimeMillis();

		return super.doTopCall(callable, cx, scope, thisObj, args);
	}
}
