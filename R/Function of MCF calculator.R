mcf_calculator <- function(
    T.sel,                 # length-12 ambient temps (°C)
    f_Tmin,                # minimum manure temp (°C)
    rm_months,             # months emptied, e.g. c(4, 9)
    
    ## ---- defaults the user may override ----
    VS_Yr  = 10000,        # kg VS per year
    VS_LQD = 100,          # % of excreted VS that enters the tank
    B0     = 0.24,         # m³ CH4 kg⁻¹ VS
    E_eff  = 95,           # emptying efficiency (%)
    
    ## ---- advanced / seldom-changed parameters ----
    f_T2d  = 3,
    f_Ea   = 19347,
    f_T1   = 308.16,
    f_R    = 1.987
) {
  ## ── 0. validate rm_months ─────────────────────────────
  if (!is.numeric(rm_months) || any(rm_months %% 1 != 0)) {
    stop("`rm_months` must be integer month numbers (1–12).")
  }
  if (any(rm_months < 1 | rm_months > 12)) {
    stop("All elements of `rm_months` must be between 1 and 12.")
  }
  if (anyDuplicated(rm_months)) {
    stop("`rm_months` must not contain duplicate months.")
  }
  if (length(rm_months) > 3) {
    stop("`rm_months` can contain at most three months.")
  }
  
  M.rm      <- rep(0L, 12)
  M.rm[rm_months] <- 1L
  Manure.rm <- rep(M.rm, 3)
  
  ## helper: Arrhenius temperature factor (van’t Hoff)
  v.hoff <- function(T_K) round(exp((f_Ea * (T_K - f_T1)) / (f_R * T_K * f_T1)), 10)
  c2k    <- function(x) x + 273.15
  
  ## ── 1. estimate manure temperature for each month ──────────
  T.m <- numeric(12)
  
  if (sum(M.rm) > 1) {                            # >1 removals → no ΔT reduction
    T.m <- pmax(T.sel, f_Tmin)
  } else if (sum(M.rm[8:12] == 1) == 1) {         # single fall removal
    T.m <- pmax(T.sel - f_T2d, f_Tmin)
  } else {                                        # no removals
    T.m <- pmax(T.sel, f_Tmin)
  }
  
  T.m <- c(T.m[12], T.m[1:11])                    # 1-month lag (IPCC 2019)
  f.m <- rep(v.hoff(c2k(T.m)), 3)                 # extend 12 → 36 months
  
  ## ── 2. CH4 simulation over 3 years ─────────────────────────
  VS_month  <- rep(VS_Yr / 12, 36)                # constant monthly input
  VS_loaded <- VS_month * (VS_LQD / 100)
  
  VS_ava <- VS_con <- CH4_m <- numeric(36)
  
  for (i in seq_len(36)) {
    if (i == 1) {                                 # first month
      VS_ava[i] <- VS_loaded[i]
    } else if (Manure.rm[i] == 0) {               # no emptying
      VS_ava[i] <- VS_loaded[i] + VS_ava[i-1] - VS_con[i-1]
    } else {                                      # emptying done
      VS_ava[i] <- VS_loaded[i] +
        (VS_ava[i-1] - VS_con[i-1]) * (1 - E_eff/100)
    }
    VS_con[i] <- VS_ava[i] * f.m[i]               # VS consumed this month
    CH4_m[i]  <- VS_con[i] * B0                   # CH4 produced this month
  }
  
  ## ── 3. MCF and output for the 3rd year ─────────────────────
  CH4_potential <- sum(VS_loaded[25:36]) * B0     # theoretical max
  CH4_year3     <- sum(CH4_m[25:36])
  MCF           <- round(CH4_year3 / CH4_potential, 6)
  
  Result<- list(
    CH4_monthly_year3 = round(CH4_m[25:36], 3),
    CH4_total_year3   = round(CH4_year3, 3),
    MCF               = MCF
  )
}

# # Example inputs (illustrative only)
# T.sel   <- c(-8.0,-5.0,0.0,5.0,12.0,18.0,21.0,20.0,15.0,8.0,0.0,-4.0)   # °C
# f_Tmin  <- 0
# M.rm    <- c(4,9)           # emptied Jan & Aug
# VS_Yr   <- 10000                                 # kg VS
# VS_LQD  <- 90                                    # %
# B0      <- 0.24                                  # m³ CH4 kg⁻¹ VS
# E_eff   <- 80                                    # %
# 
# mcf_calculator(T.sel, f_Tmin, M.rm)
