# Endogenní ι a λ: implementace modelu v R

Implementace vychází z `note4_scale_model.pdf` (srpen 2026) a napojuje se na objekty v přiloženém `05-script.R`. Výpočet používá pouze base R. Všechny podíly vstupují jako desetinná čísla; výstupní dopady do GDP jsou v procentech.

## Rychlé napojení na projekt

1. Rozbal složku `ai_scale_model` do pracovního adresáře `calc` svého projektu.
2. Spusť původní skript až po vytvoření objektů `tfp` a `gdp`, tedy již po převodu `AISA_bill_weighted` a `Lshare` na desetinné podíly. V dodaném skriptu je vhodné místo před blokem `Figure: US TFP`.
3. Vlož:

```r
source("ai_scale_model/integrate_project.R")
```

Vzniknou následující objekty:

| Objekt | Obsah |
|---|---|
| `scale_fit` | Kalibrované parametry a odchylky od cílových momentů |
| `scale_tiva` | Endogenní ι a λ podle sektoru a schopnosti AI |
| `tfp_endogenous` | Původní sektorová tabulka doplněná o nové výsledky |
| `gdp_endogenous` | Výsledky jednotlivých zemí, včetně srovnatelného modelu s konstantními podíly |
| `global_endogenous` | Globální výsledky vážené GDP jednotlivých zemí |

Hlavní výstup pro tvůj globální graf je `global_endogenous$gdp_endo_adj_internet`. Srovnávací konstantní model je `gdp_frozen_adj_internet`. Původní `tfp` a `gdp` zůstávají dostupné. Samotné načtení modulu přes `source("ai_scale_model/ai_scale_model.R")` nic nepočítá ani nemění.

## Parametry a ekonomický význam

| Symbol | R | Význam |
|---|---|---|
| κ | `kappa` | Schopnost AI na stejné škále jako obtížnost práce |
| d | `difficulty` | Obtížnost kohorty práce |
| z = κ − d | `z` | Převaha schopnosti AI nad obtížností |
| W | implicitní rozdělení | Mzdový objem, který obslouží jedno nasazení AI |
| ν | implicitní v `mu0` | Medián logaritmu W; medián W je exp(ν) |
| F | implicitní v `mu0` | Fixní náklad nasazení ve stejných jednotkách a za stejné období jako W |
| b | `b` | Parametr rozptýlení Laplaceova rozdělení log W; musí být mezi 0 a 1 |
| θ | `theta` | Rychlost poklesu variabilních nákladů s růstem z |
| μ₀ = ν − ln F | `mu0` | Logaritmus poměru mediánového mzdového objemu k fixnímu nákladu |
| γ | `gamma` | Hrubá úspora před fixním nákladem, `1 - exp(-theta*z)` pro z > 0 |
| m = μ₀ + ln γ | `m` | Logaritmus poměru hrubé úspory mediánového nasazení k jeho fixnímu nákladu |
| ι | `iota` | Podíl mzdového objemu, který se vyplatí automatizovat |
| λ | `lambda` | Čistá úspora na dolar rentabilně automatizovaného mzdového objemu |
| π = ι × λ | `pi` | Čistá úspora na dolar celé kohorty |

λ zahrnuje odpočet fixního nákladu. Nejde o úsporu času před zaplacením systému. Funkce `scale_cohort()` implementuje obě větve Proposition 1; žádná další fixní srážka se pak při agregaci nepřidává. Pro z ≤ 0 nastavuje úspory na nulu.

## Jak se endogenizace napojuje na AISA

Při každém růstu κ přibývá v AISA nově dosažitelná práce. Přírůstek kumulativní expozice vyjadřuje mzdovou váhu příslušného intervalu obtížnosti. `scale_from_cdf()` tuto hmotu rozloží uvnitř intervalu a `scale_aggregate()` sečte výsledky jednotlivých kohort.

Výchozí integrace předpokládá rovnoměrné rozdělení obtížnosti mezi pozorovanými body AISA. Používá 16 kvadraturních bodů na interval; nejde o další ekonomický parametr. Na původních bodech škály se přesně zachovává pozorovaná expozice, až na numerické zaokrouhlení. Mezi nimi je rekonstrukce aproximací. Zvýšení počtu kvadraturních bodů zlepšuje integraci uvnitř zvoleného rozdělení, nikoliv identifikaci skutečné obtížnosti.

Pokud křivka začíná při κ = 1 s kladnou expozicí, je nepozorovaná první kohorta v integračním skriptu výslovně rozložena mezi 0 a 1. Modul na tento předpoklad upozorní. Pro článek doporučuji jemnější škálu od nuly nebo původní obtížnosti a kontrolu citlivosti na `allocation = "upper"`, která přírůstky umístí na pravé hranice intervalů. Při porovnávání alternativ rozdělení znovu kalibruj parametry.

Z křivek po κ = 6 se neodvozuje žádná další nově exponovaná práce. Vyšší κ lze vyhodnotit pouze jako prohlubování schopností vůči již zahrnutým kohortám. Příklady s κ = ∞ jsou limity tohoto modelu, ne prognózy dosažení takové technologie.

Původní AISA ve `work1207.pdf`, oddílech 3.2–3.3, kombinuje požadované úrovně **dovedností**, jejich důležitost a podíl práce na počítači. Rekonstruované kohorty proto interpretuj jako obtížnostní strukturu implicitní v AISA. Samotný index neposkytuje pozorovanou obtížnost každého jednotlivého O*NET tasku. Přiložená poznámka tento přechod k taskům popisuje silněji, než umožňují samotná data AISA.

Klesající AISA, měnící se mzdový vzorek mezi schopnostmi a duplicitní řádky způsobí chybu. Kód je automaticky nevyhlazuje ani neopravuje. Nepřidává ani další násobení obálkou počítačové práce, například 0,52: ta je již obsažena v AISA.

## Kalibrace

Výchozí napojení fixuje b = 0,7 a při κ = 2,6 hledá θ a μ₀ tak, aby americký agregát splnil ι = 0,49 a λ = 0,33. Používá kompenzační váhy odvozené přímo z amerických sektorových podílů na VA a sektorových podílů práce v `tfp`. Díky tomu při čistém cíli λ reprodukuje přesně americký benchmark tvého srovnatelného konstantního modelu na stejném vzorku.

Jde o **jednu společnou kalibraci**, nikoliv samostatné vynucení 0,49 a 0,33 v každém odvětví nebo zemi. Mezi odvětvími se liší obtížnostní struktura a tím i endogenní podíly. Globální výsledek v benchmarku se proto nemusí rovnat původnímu globálnímu výsledku. Rozdíly v lokálních mzdách a nákladech nasazení nejsou nově odhadnuty; pro takové rozšíření jsou potřeba další data.

V `integrate_project.R` lze změnit cíle nebo zvolit původní kalibraci 0,23 a 0,27. Pro citlivost na b změň jeho hodnotu například na 0,6 nebo 0,75 a oba vnitřní parametry znovu dopočítej. Dva momenty samy neidentifikují všechny tři parametry.

| Režim | Co se kalibruje |
|---|---|
| `saving_measure = "net"` | Doslovná specifikace poznámky: cílová úspora je λ po fixních nákladech |
| `saving_measure = "gross"` | Cílová úspora je γ vážená rentabilně automatizovaným mzdovým objemem; čistá λ vyjde nižší |

Použití reportované časové úspory 0,33 jako **čisté** λ je dodatečný identifikační předpoklad poznámky. Kód jej ve výchozím nastavení replikuje, ale neověřuje empirickou rovnocennost těchto momentů. Ani přepnutí na hrubou úsporu samo nevyřeší rozdíl mezi váhami respondentů a vahami mzdového objemu.

`scale_preset()` zpřístupňuje zaokrouhlené parametry z PDF:

| Scénář | b | θ | μ₀ |
|---|---:|---:|---:|
| `"acemoglu"` | 0,7 | 0,48 | −1,74 |
| `"updated"` | 0,7 | 0,65 | −0,20 |
| `"platform"` | 0,7 | 0,48 | 0,10 |

Tyto předvolby slouží k reprodukci kohortových výsledků. Bez původního rozdělení obtížnosti nemusí vracet původní agregátní kalibrační momenty. Pro tvůj projekt je vhodný výchozí nový fit.

## Správná agregace

Označme `weight` podíl kohorty na **celém sektorovém mzdovém objemu**. Pak:

```r
exposure     <- sum(weight[difficulty <= kappa])
viable_share <- sum(weight * cohort_iota)
saving_share <- sum(weight * cohort_iota * cohort_lambda)

iota_sector   <- viable_share / exposure
lambda_sector <- saving_share / viable_share
tfp_sector    <- saving_share * Lshare
```

`cohort_iota` a `cohort_lambda` jsou výsledky pro každou kohortu; `Lshare` je sektorový podíl práce na VA. Podíly s nulovým jmenovatelem modul eviduje jako nulu a u λ nastaví `lambda_defined = FALSE`.

**Neprůměrovat λ prostým průměrem a nenásobit `saving_share` znovu AISA.** Expozice je v něm již zahrnuta. Přesně platí `saving_share = exposure * iota * lambda`.

Sektorové dopady do TFP se váží podíly na VA. Teprve potom se provede národní převod na GDP a případné násobení `internet_use`. Globální dopad je následně průměrem **hotových národních dopadů**, váženým GDP z roku 2023. Poměr samostatných globálních průměrů těchto veličin obecně dává jiný výsledek; tento postup z některých srovnávacích bloků původního skriptu zde nepřebírám.

Převod TFP na GDP zachovává tvoji konvenci dělení národním podílem práce. V poznámce má původní tabulka Acemoglua jiný převod 1/0,57. Tyto dvě konvence nelze zaměnit při porovnávání čísel.

Výchozí režim při chybějících vstupech zastaví výpočet. `scale_missing = "common_sample"` umožňuje srovnání obou modelů na shodných dostupných sektorech a vrací `va_share_available`, `va_share_used` a `missing_va_share`. Národní podíl práce i TFP používají tentýž vzorek. Je-li pokrytí neúplné, přenesení poměru na celé GDP implicitně předpokládá reprezentativnost pokryté části. Zkontroluj pokrytí i stabilitu sektorového vzorku přes schopnosti; nepovažuj chybějící odvětví automaticky za nulový přínos.

`internet_use` zůstává tvým vnějším indexem digitální připravenosti. Je to poměr vůči USA, může tedy přesáhnout 1. Po endogenizaci ι jej již neinterpretuj jako další odhad téhož podílu ekonomicky rentabilních nasazení; jinak by se mohl překrývat tentýž mechanismus.

U jednotlivé kohorty rostou ι a λ se schopností AI. Jejich **podmíněné sektorové průměry** růst nutně nemusí: příchod obtížných kohort může průměr snížit. Model zaručuje růst celkové úspory při pevných vahách, nikoliv monotonicitu všech agregovaných podílů. Kód takový růst uměle nevynucuje.

## Přímé použití s původními daty

Pokud máš skutečné obtížnosti a váhy, rekonstrukci AISA nepotřebuješ:

```r
# difficult_tasks obsahuje obtížnost a váhu každé kohorty v daném sektoru.
# Je-li omega váha normalizovaná uvnitř počítačové práce a Omega její podíl
# na celé sektorové kompenzaci, celková váha je Omega * omega.
tasks <- scale_tasks(
  difficulty = difficult_tasks$difficulty,
  weight     = difficult_tasks$Omega * difficult_tasks$omega,
  sector     = difficult_tasks$sector
)

# us_sector_weights: pojmenovaný vektor podílů sektorů na americkém mzdovém
# objemu; součet 1, přesně stejné sektory jako v tasks.
national_tasks <- scale_pool(tasks, us_sector_weights)
fit <- scale_calibrate(national_tasks, kappa0 = 2.6,
                       b = 0.7, target_iota = 0.49, target_lambda = 0.33)
sector_results <- scale_aggregate(tasks, seq(0, 6, .1), fit$pars)
```

Nákladový scénář je samostatný zásah: `scale_change_cost(fit$pars, factor = 0.2)` sníží F na pětinu, drží θ fixní a zvýší μ₀ o ln 5. Nejde o zvýšení κ a v tomto kontrafaktuálu znovu nekalibruj na původní adopční momenty.

## Reprodukce a ověření

```r
source("ai_scale_model/replicate_note.R") # obrázky 1–4 a kohortové tabulky
```

Testy spouštěj v samostatné R relaci: obsahují syntetické objekty pojmenované jako objekty projektu.

```sh
Rscript ai_scale_model/test_scale_model.R
```

Ověřeno v R 4.2.2: 95 kontrol, včetně přímé numerické integrace původních definic v obou větvích, spojitosti, limitů, kalibrace, rekonstrukce expozice a spuštění celého integračního skriptu. Maximální absolutní rozdíl proti integraci byl 6,05 × 10⁻¹⁰; chyba při zpětné obnově známých parametrů byla 1,75 × 10⁻¹¹. Podrobný záznam je ve `validation_report.txt`.

Pro příklad v oddílu 6 vychází při z = 1 a původní kalibraci γ = 0,381217, ι = 0,266724, λ = 0,266852 a π = 0,071176. Jde o jednu obtížnostní kohortu, nikoliv o americký průměr. Obrázek 1 je nově vykreslená ilustrace stejného mechanismu; hustota v pravém panelu je hustotou počtů nasazení, takže její stínovaná plocha sama není mzdově váženou ι.

**Rozsah replikace:** vzorce, kohortové výsledky, obrázky a kalibrační/integrační postup jsou reprodukované. Přiložené soubory neobsahují původní obtížnosti a kompenzační váhy z poznámky ani externí datové soubory, které načítá `05-script.R`. Proto nejsou ověřeny její agregátní odhady 9,3 %, přibližně 20 % a 21 % ani nové empirické výsledky pro jednotlivé země. Syntetické výsledky v testovacím protokolu jsou pouze kontrolou implementace.

Funkce `scale_exponential()` navíc implementuje integrál pro exponenciální obtížnost z Proposition 2. Zde η (`eta`) označuje intenzitu exponenciálního rozdělení, jehož střední obtížnost je 1/η. Integrál se vyhodnocuje přímo: druhý parametr neúplné beta funkce v PDF je záporný, takže standardní R `pbeta()` není pro tento zápis použitelná.
