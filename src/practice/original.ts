
interface Env { taxRate: number; currency: string; }
interface CartState { items: string[]; totalTax: number; }

type Reader<Env, A> = (env: Env) => A;
type State<S, A>    = (s: S) => [A, S];

// 결합: Env를 읽고 CartState를 갱신하며 값을 돌려줌
type RState<A> = Reader<Env, State<CartState, A>>;

// ---------- 기본 조합자 ----------
const ask: RState<Env> = env => s => [env, s];
const get: RState<CartState> = env => s => [s, s];
const put = (s: CartState): RState<undefined> =>
  _env => _old => [undefined, s];
const modify = (f: (s: CartState) => CartState): RState<undefined> =>
  env => s => [undefined, f(s)];

// ---------- 실제 로직 ----------
const addItem = (item: string, price: number): RState<string> =>
  env => s => {
    const tax = price * env.taxRate;
    return [`${item}${env.currency}`,
            { items: [...s.items, item], totalTax: s.totalTax + tax }];
  };

const printReceipt: RState<string> =
  env => s =>
    `${s.items.join(", ")} | 세금 합계: ${s.totalTax.toFixed(2)}${env.currency}`;

// ---------- 실행 ----------
const emptyCart: CartState = { items: [], totalTax: 0 };
const myEnv: Env = { taxRate: 0.1, currency: "€" };

const step1 = addItem("coffee", 4)(myEnv)(emptyCart);
const step2 = addItem("milk", 2)(myEnv)(step1[1]);
console.log(printReceipt(myEnv)(step2[1])[0]);
// => "coffee€, milk€ | 세금 합계: 0.60€"
