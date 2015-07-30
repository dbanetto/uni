# NWEN242 Homework Assignment

## David Barnett - ID: 300313764

### Question 1

#### Part A

If you improved the run time of divide by three times in, a $100ms$ total time execution
of the program given that the divide take $20\%$ of that time thus $20ms$. The improvement
would yield:

$$20ms \div 3 = 6 \frac{2}{3}ms$$

A decrease in time for the division from $20ms$ to $6 \frac{2}{3}ms$ making
a total improvement of $1.15$ (rounded 2.d.p) times faster than before.

$$\frac{100ms}{80ms + 6 \frac{2}{3}ms} = 1.1538...$$


If you improved the run time of divide by three times in, a $100ms$ total time execution
of the program given that the divide take $50\%$ of that time thus $50ms$. The improvement
would yield:

$$\frac{50ms}{8} = 6.25ms$$

A decrease in time for the division from $50ms$ to $6.25ms$ making
a total improvement of $1.78$ (rounded 2.d.p) times faster than before.

$$\frac{100ms}{50ms + 6.25ms} = 1 \frac{7}{9}$$

\pagebreak

This shows that the target of a $1.4$ times faster program is possible. But to get
$1.4$ time exactly the multiply operation would have to be below the maximum improvement
of 8 times better:

Let $x$ be the target time for multiply needs to make the total execution time $1.4$ times
faster

$$1.4 = \frac{100ms}{50ms+x}$$
$$70ms + 1.4x = 100ms$$
$$1.4x = 30ms$$
$$x = \frac{150}{7}ms$$

Let $t$ be the target improvement to multiple needed to achieve the target time $x$

$$\frac{50}{t} = x$$
$$x*t = 50 $$
$$t = \frac{50}{x}$$
$$t = 50 \div \frac{150}{7}$$
$$t = \frac{7}{3} = 2.33 (2.d.p) $$

To make the improvements Management wants it would take a $2.33$ times improvement to
the multiply operation to achieve the overall goal of $1.4$ times faster program.


#### Part B

Applying both the 8 times and 3 time improvements for the multiply and divide operations  respectfully would get:

$$\frac{100ms}{30ms + 6.25ms + 6\frac{2}{3}ms} = 2.33 (2.d.p)$$

A $2.33$ times improvement relative to the original machine in total execution time.

\pagebreak

### Question 2

#### Part A

$$IPS_{P1} = \frac{3GHz}{1.5} = 2,0000,00$$
$$IPS_{P2} = \frac{2.5GHz}{1.0} = 2,5000,00$$
$$IPS_{P3} = \frac{4GHz}{2.2} = 1,818,181$$

Processor $P2$ has the highest instructions per second


$$Total\ Instructions_{P1} = 10 * IPS_{P1} = 10 * 2,0000,00 = 20,000,000$$
$$Total\ Instructions_{P2} = 10 * IPS_{P2} = 10 * 2,5000,00 = 25,000,000$$
$$Total\ Instructions_{P3} = 10 * IPS_{P3} = 10 * 1,818,181 = 18,1818,18$$
