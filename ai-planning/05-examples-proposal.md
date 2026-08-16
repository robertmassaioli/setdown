# Examples Proposal

**Date:** 2026-04-13

The existing `examples/standard/example.setdown` uses abstract names (`A`, `B`, `C`) and
references files that do not exist. It demonstrates syntax but nothing a new user could run or
relate to. The proposals below each include concrete, self-contained input files with realistic
content and a `.setdown` file that tells a coherent story.

---

## The 10 Candidates

### 1. `basic-intersection` — Fruit lists in common

Two shopping lists. Find items on both.

**Input files:** `list-a.txt`, `list-b.txt` (10–12 fruit/grocery names each, several overlapping)
**Definitions:** one — `InBoth: "list-a.txt" /\ "list-b.txt"`
**Complexity:** minimal  
**Why it's useful:** the canonical "first example" — one file, one operator, immediately
understandable result.

---

### 2. `basic-difference` — What changed between two releases

Two text files listing the endpoints of an API at v1 and v2. Find added and removed endpoints.

**Input files:** `api-v1.txt`, `api-v2.txt`
**Definitions:**
```
Added:   "api-v2.txt" - "api-v1.txt"
Removed: "api-v1.txt" - "api-v2.txt"
```
**Complexity:** low  
**Why it's useful:** the difference operator is the most practically useful one. An API diff is
immediately relatable to any developer and shows that setdown can answer real questions.

---

### 3. `mailing-lists` — Merge and segment subscriber lists

Three newsletter lists (announcements, engineering, product). Find subscribers on all three,
subscribers unique to each list, and a master unsubscribe list.

**Input files:** `announcements.txt`, `engineering.txt`, `product.txt`, `unsubscribed.txt`
**Definitions:**
```
AllSubscribers:    ("announcements.txt" \/ "engineering.txt") \/ "product.txt"
EngAndProduct:     "engineering.txt" /\ "product.txt"
ActiveSubscribers: AllSubscribers - "unsubscribed.txt"
```
**Complexity:** medium (three inputs, three definitions, one referencing another)  
**Why it's useful:** the union operator gets its showcase here; chaining three files with `\/`
also introduces mandatory bracketing naturally.

---

### 4. `data-reconciliation` — Compare two exports of the same dataset

Two CSV-style exports of customer IDs (one from last month, one from this month). Identify
new customers, lost customers, and retained customers.

**Input files:** `customers-jan.txt`, `customers-feb.txt`
**Definitions:**
```
Retained: "customers-jan.txt" /\ "customers-feb.txt"
New:      "customers-feb.txt" - "customers-jan.txt"
Lost:     "customers-jan.txt" - "customers-feb.txt"
```
**Complexity:** low–medium (two inputs, three definitions)  
**Why it's useful:** the most archetypal real-world use case for set operations on line-based
data. The three definitions together form a complete picture, showing how named results
complement each other.

---

### 5. `access-control` — Derive permission groups from role membership

Three role files (admins, developers, contractors). Derive combined groups: internal staff,
all users, and users who have elevated access.

**Input files:** `admins.txt`, `developers.txt`, `contractors.txt`
**Definitions:**
```
InternalStaff:   "admins.txt" \/ "developers.txt"
AllUsers:        InternalStaff \/ "contractors.txt"
ElevatedAccess:  "admins.txt" \/ "developers.txt"
ContractorsOnly: "contractors.txt" - InternalStaff
```
**Complexity:** medium (three inputs, four definitions, cross-referencing named results)  
**Why it's useful:** shows referencing named definitions in other expressions — the feature
that lifts setdown above a simple one-shot tool. Access control is a universal domain.

---

### 6. `software-dependencies` — Audit shared and unique dependencies

Two applications' dependency lock files (one line per package name). Find shared dependencies,
dependencies unique to each app, and the full combined set for a monorepo manifest.

**Input files:** `app-a.txt`, `app-b.txt`
**Definitions:**
```
Shared:       "app-a.txt" /\ "app-b.txt"
OnlyInA:      "app-a.txt" - "app-b.txt"
OnlyInB:      "app-b.txt" - "app-a.txt"
AllDeps:      "app-a.txt" \/ "app-b.txt"
```
**Complexity:** low–medium (two inputs, four definitions)  
**Why it's useful:** immediately relevant to developers; demonstrates all three operators in
one file; the four definitions together answer a complete dependency audit question.

---

### 7. `feature-flags` — Segment users by feature exposure

Four user-ID lists representing users who were exposed to different experiments. Find users in
multiple experiments and users to exclude from a follow-up study.

**Input files:** `experiment-a.txt`, `experiment-b.txt`, `experiment-c.txt`, `opted-out.txt`
**Definitions:**
```
ABOverlap:        "experiment-a.txt" /\ "experiment-b.txt"
AllExposed:       ("experiment-a.txt" \/ "experiment-b.txt") \/ "experiment-c.txt"
EligibleFollowup: AllExposed - "opted-out.txt"
CoreGroup:        ("experiment-a.txt" /\ "experiment-b.txt") /\ "experiment-c.txt"
```
**Complexity:** medium–high (four inputs, four definitions, deeper chaining)  
**Why it's useful:** A/B testing and experiment analysis is a common data engineering task;
introduces four-input complexity and a realistic exclusion list.

---

### 8. `log-analysis` — Cross-reference server access logs

Three files of IP addresses extracted from access logs for different services. Find IPs that
hit multiple services (potential scrapers or power users) and IPs exclusive to one service.

**Input files:** `service-a-ips.txt`, `service-b-ips.txt`, `service-c-ips.txt`, `known-bots.txt`
**Definitions:**
```
HitAandB:       "service-a-ips.txt" /\ "service-b-ips.txt"
HitAll:         HitAandB /\ "service-c-ips.txt"
SuspiciousIPs:  HitAll - "known-bots.txt"
OnlyServiceA:   "service-a-ips.txt" - ("service-b-ips.txt" \/ "service-c-ips.txt")
```
**Complexity:** high (four inputs, four definitions, nested expressions)  
**Why it's useful:** operations on log data is a classic sysadmin use case; introduces nested
bracket expressions and multi-level chaining.

---

### 9. `symmetric-difference` — Manually implement A △ B

Since setdown has no built-in symmetric difference operator, this example shows how to compose
the three primitives to achieve it, with an explanatory comment in the `.setdown` file.

**Input files:** `set-a.txt`, `set-b.txt`
**Definitions:**
```
-- Symmetric difference: elements in A or B, but not both
-- Implemented as (A - B) \/ (B - A) since setdown has no △ operator
OnlyInA:  "set-a.txt" - "set-b.txt"
OnlyInB:  "set-b.txt" - "set-a.txt"
SymDiff:  OnlyInA \/ OnlyInB
```
**Complexity:** low (two inputs, three definitions)  
**Why it's useful:** teaches users how to compose operators to express derived operations;
also implicitly documents the missing operator, which may prompt contribution.

---

### 10. `team-projects` — Multi-team project assignment audit

Five files: three team membership lists and two project contributor lists. Derive who on each
team contributes to each project, who contributes to both projects, and unassigned engineers.

**Input files:** `team-alpha.txt`, `team-beta.txt`, `team-gamma.txt`, `project-x.txt`,
`project-y.txt`
**Definitions:**
```
AllEngineers:      ("team-alpha.txt" \/ "team-beta.txt") \/ "team-gamma.txt"
BothProjects:      "project-x.txt" /\ "project-y.txt"
AlphaOnX:         "team-alpha.txt" /\ "project-x.txt"
CrossTeamOnY:     ("team-alpha.txt" \/ "team-beta.txt") /\ "project-y.txt"
Unassigned:       AllEngineers - ("project-x.txt" \/ "project-y.txt")
```
**Complexity:** high (five inputs, five definitions, multiple levels of nesting)  
**Why it's useful:** the most complex example; shows that setdown scales gracefully to
non-trivial organisation problems and exercises every language feature.

---

## Recommendation: implement these five

| Example | Reason |
|---------|--------|
| **2. `basic-difference`** | The difference operator is the most practically useful and the easiest to build intuition around. Every new user will immediately understand "what was removed between v1 and v2". Showcases a single operator cleanly. |
| **4. `data-reconciliation`** | The canonical real-world use case for line-based set operations. Three definitions (Retained, New, Lost) form a self-contained, complete story from two inputs. Low complexity, high relatability. |
| **5. `access-control`** | The first example that chains named definitions — `ContractorsOnly` references `InternalStaff`. This is the feature that makes setdown more than a one-shot tool, and access control is universally understood. |
| **6. `software-dependencies`** | Directly relevant to the target audience (developers). Demonstrates all three operators in one compact file, giving the broadest operator coverage for the fewest lines. |
| **8. `log-analysis`** | The most complex recommended example. Introduces nested bracket expressions, a four-input exclusion pattern, and multi-level chaining. Demonstrates that setdown handles genuinely non-trivial problems without becoming unwieldy. |

**Not recommended for now:**
- `basic-intersection` (1) — too similar to `data-reconciliation`; the fruit domain feels toy-ish next to a realistic use case
- `mailing-lists` (3) — the bracket-chaining point is already covered by `access-control`
- `feature-flags` (7) — good but overlaps heavily with `log-analysis` in structure
- `symmetric-difference` (9) — useful but feels like a workaround showcase rather than a strength showcase; better deferred until the native operator exists
- `team-projects` (10) — excellent capstone but five input files is a lot to absorb; better added after the five above establish the baseline
