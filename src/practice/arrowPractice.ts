// Arrow practice in TypeScript — no dependencies.
// Run: npx tsx src/practice/arrowPractice.ts
//
// An Arrow is just a wrapper around a function. All combinators below
// are built from one primitive: arr.

export type Arrow<A, B> = { readonly run: (a: A) => B };

// ---------- primitive (given) ----------

export const arr = <A, B>(f: (a: A) => B): Arrow<A, B> => ({ run: f });

// ---------- combinators (given as worked examples) ----------

// (>>>) : pipe two arrows
export const compose = <A, B, C>(f: Arrow<A, B>, g: Arrow<B, C>): Arrow<A, C> =>
  arr((a) => g.run(f.run(a)));

// first : apply to the left of a pair
export const first = <A, B, C>(f: Arrow<A, B>): Arrow<[A, C], [B, C]> =>
  arr(([a, c]) => [f.run(a), c] as [B, C]);

// ---------- practice: implement these ----------

// second : apply to the right of a pair
export const second = <A, B, C>(f: Arrow<A, B>): Arrow<[C, A], [C, B]> => {
  throw new Error("TODO: implement second");
};

// split (***) : run two arrows on a pair
export const split = <A, B, C, D>(
  f: Arrow<A, B>,
  g: Arrow<C, D>,
): Arrow<[A, C], [B, D]> => {
  throw new Error("TODO: implement split");
};

// fanout (&&&) : run two arrows on the same input
export const fanout = <A, B, C>(
  f: Arrow<A, B>,
  g: Arrow<A, C>,
): Arrow<A, [B, C]> => {
  throw new Error("TODO: implement fanout");
};

// swap : exchange the pair
export const swap = <A, B>(): Arrow<[A, B], [B, A]> => {
  throw new Error("TODO: implement swap");
};

// uncurry on pairs: use f, g, and arr only. No plain closures on inputs!
export const sumPair = (): Arrow<[number, number], number> => {
  throw new Error("TODO: implement sumPair using first/second/split/swap");
};

// ---------- tests ----------

const inc = arr((n: number) => n + 1);
const dbl = arr((n: number) => n * 2);

type Case = [string, () => unknown, unknown];
const cases: Case[] = [
  ["compose inc then dbl", () => compose(inc, dbl).run(3), 8],
  ["first inc", () => first(inc).run([3, "x"]), [4, "x"]],
  ["second dbl", () => second(dbl).run(["x", 3]), ["x", 6]],
  ["split", () => split(inc, dbl).run([3, 4]), [4, 8]],
  ["fanout", () => fanout(inc, dbl).run(3), [4, 6]],
  ["swap", () => swap<number, string>().run([1, "a"]), ["a", 1]],
  ["sumPair", () => sumPair().run([2, 3]), 5],
];

for (const [name, thunk, want] of cases) {
  try {
    const got = thunk();
    console.log(
      JSON.stringify(got) === JSON.stringify(want)
        ? `PASS  ${name}`
        : `FAIL  ${name}: got ${JSON.stringify(got)}, want ${JSON.stringify(want)}`,
    );
  } catch (e) {
    console.log(`TODO  ${name} (${(e as Error).message})`);
  }
}
