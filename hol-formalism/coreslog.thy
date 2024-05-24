theory coreslog
  imports extras Main HOL.String HOL.Set HOL.List 

begin

section \<open>General definitions\<close>

type_synonym Tag = string
type_synonym Var = string

datatype Subcl = Rel Tag "Subcl list" | Nat nat | String string | Var Var
datatype Clause = Rel Tag "Subcl list"

record Rule =
  Head :: Clause
  Body :: "Clause set"

type_synonym Program = "Rule set"

type_synonym DB = "Clause set"

subsection \<open>ground clauses\<close>

fun ground_subcl :: "Subcl \<Rightarrow> bool"  where
"ground_subcl (Subcl.Rel tag subcls) = list_all ground_subcl subcls" |
"ground_subcl (Nat _) = True" |
"ground_subcl (String _) = True" |
"ground_subcl (Var _) = False"

fun ground_clause :: "Clause \<Rightarrow> bool" where
"ground_clause (Rel tag subcls) = list_all ground_subcl subcls"

definition ground_db :: "DB \<Rightarrow> bool" where
[simp]: "ground_db db \<equiv> \<forall> cl \<in> db. ground_clause cl"

lemma ground_db_empty : "ground_db {}"
  unfolding ground_db_def by simp

subsection \<open>substitutions\<close>

fun substitute_subcl :: "(Var \<rightharpoonup> Subcl) \<Rightarrow> Subcl \<Rightarrow> Subcl" where
"substitute_subcl \<Theta> (Subcl.Rel tag subcls) = Subcl.Rel tag (map (substitute_subcl \<Theta>) subcls)" |
"substitute_subcl \<Theta> (Nat n) = (Nat n)" |
"substitute_subcl \<Theta> (String str) = (String str)" |
"substitute_subcl \<Theta> (Var var) = (case (\<Theta> var) of 
                                      Some v \<Rightarrow> v 
                                     |None \<Rightarrow> (Var var))"

fun substitute_clause :: "(Var \<rightharpoonup> Subcl) \<Rightarrow> Clause \<Rightarrow> Clause" where
"substitute_clause \<Theta> (Clause.Rel tag subcls) = Clause.Rel tag (map (substitute_subcl \<Theta>) subcls)"

abbreviation subs_cl :: "Clause \<Rightarrow> (Var \<rightharpoonup> Subcl) \<Rightarrow> Clause" ("_\<lbrakk>_\<rbrakk>" [50,50] 100) where
"cl\<lbrakk>\<Theta>\<rbrakk> \<equiv> substitute_clause \<Theta> cl" 

(* Unused *)
typedef Fact ="{cl. ground_clause cl}"
proof -
  have "\<exists> x::Tag . True" by simp
  then obtain tag :: Tag where "True" by simp
  have "ground_clause (Clause.Rel tag [])" by simp
  thus ?thesis by blast
qed

subsection \<open>Valid programs\<close>
fun subcl_vars :: "Subcl \<Rightarrow> Var list" where
"subcl_vars (Subcl.Rel tag subcls) = concat (map subcl_vars subcls)" | 
"subcl_vars (Nat n) = []" |
"subcl_vars (String str) = []" |
"subcl_vars (Var v) = [v]" 

fun clause_vars :: "Clause \<Rightarrow> Var list" where
"clause_vars (Clause.Rel tag subcls) = concat (map subcl_vars subcls)"

definition valid_rule :: "Rule \<Rightarrow> bool" where
"valid_rule r \<equiv> set (clause_vars (Head r)) \<subseteq> \<Union> {set (clause_vars b) | b. b \<in> Body r}"

definition valid_program :: "Program \<Rightarrow> bool" where
[simp]: "valid_program p \<equiv> \<forall> r \<in> p. valid_rule r"

subsection \<open>unrolling and subfact-closure\<close>

fun unroll_subcl :: "Subcl \<Rightarrow> Clause list" where
"unroll_subcl (Subcl.Rel tag items) = Cons (Clause.Rel tag items) (concat (map unroll_subcl items))" |
"unroll_subcl (Nat _) = []" |
"unroll_subcl (String _) = []" |
"unroll_subcl (Var _) = []"

fun unroll :: "Clause \<Rightarrow> Clause list" where
"unroll (Clause.Rel tag items) = Cons (Clause.Rel tag items) (concat (map unroll_subcl items))"

definition subfact_closed :: "DB \<Rightarrow> bool" where
"subfact_closed db \<equiv> \<forall> cl \<in> db. set (unroll cl) \<subseteq> db"

lemma subfact_closed_union:
"subfact_closed db1 \<and> subfact_closed db2 \<Longrightarrow> subfact_closed (db1 \<union> db2)"
  unfolding subfact_closed_def
  by auto

lemma subfact_closed_union2:
"\<forall> x \<in> X. subfact_closed x \<Longrightarrow> subfact_closed (\<Union> X)"
  unfolding subfact_closed_def
  by auto

lemma subfact_closed_intersection:
"subfact_closed db1 \<and> subfact_closed db2 \<Longrightarrow> subfact_closed (db1 \<inter> db2)"
  unfolding subfact_closed_def
  by simp

lemma subfact_closed_intersection2:
"\<forall> x \<in> X. subfact_closed x \<Longrightarrow> 
 subfact_closed (\<Inter> X)"
  unfolding subfact_closed_def
  by auto

lemma subfact_closed_empty: "subfact_closed {}"
  unfolding subfact_closed_def
  by auto

lemma subfact_closed_top: "subfact_closed top"
  unfolding subfact_closed_def
  by simp

section \<open>Fixed Point Semantics\<close>

definition ICr :: "Rule \<Rightarrow> DB \<Rightarrow> DB" where
"ICr rule db = db \<union> \<Union> {set (unroll (substitute_clause \<Theta> (Head rule))) | 
                      \<Theta>. \<forall> b \<in> (Body rule). substitute_clause \<Theta> b \<in> db}"

definition ICp :: "Program \<Rightarrow> DB \<Rightarrow> DB" where
"ICp program db = db \<union>  \<Union> {ICr r db| r. r \<in> program}"

lemma mono_ICr : "mono (ICr R)"
  unfolding ICr_def mono_def
  by blast

lemma mono_ICp : "mono (ICp P)"
  using mono_ICr 
  unfolding ICp_def mono_def 
  apply (simp only:Union_eq)
  apply auto
  apply (subgoal_tac "\<exists>r. r \<in> P \<and> xa \<in> ICr r y")
   apply auto[1]
  by blast

lemma ground_substitutions_ground_subcl :
"\<forall> var \<in> set (subcl_vars cl). (\<exists> cl. \<Theta> var = (Some cl) \<and> ground_subcl cl) \<Longrightarrow>
 ground_subcl (substitute_subcl \<Theta> cl)"
  apply (induction cl rule:substitute_subcl.induct)
     defer
     apply simp
    apply simp
  apply auto[1]
  apply (simp only:substitute_subcl.simps(1) ground_subcl.simps(1) subcl_vars.simps(1))
  apply (auto)
  apply (subst List.Ball_set[THEN sym])
  by simp

lemma ground_substitutions_ground_clause :
"\<forall> var \<in> set (clause_vars cl). (\<exists> cl. \<Theta> var = (Some cl) \<and> ground_subcl cl) \<Longrightarrow>
 ground_clause (substitute_clause \<Theta> cl)"
  apply (induction cl rule:clause_vars.induct)
  apply (simp only:clause_vars.simps substitute_clause.simps ground_clause.simps)
  apply auto
  using ground_substitutions_ground_subcl
  apply (subst Ball_set[THEN sym])
  by simp

lemma substituted_subcl_ground_substitution_ground:
"ground_subcl (substitute_subcl \<Theta> cl) \<Longrightarrow>
 \<forall> var \<in> set (subcl_vars cl).(\<exists> cl'. \<Theta> var = (Some cl') \<and> ground_subcl cl')"
  apply (induction cl rule:substitute_subcl.induct)
     defer
     apply simp
    apply simp
  apply simp
   apply (metis ground_subcl.simps(4) option.case_eq_if option.exhaust_sel)
  apply simp
  by (metis Ball_set image_eqI set_map)

lemma substituted_clause_ground_substitution_ground:
"ground_clause (substitute_clause \<Theta> cl) \<Longrightarrow>
 \<forall> var \<in> set (clause_vars cl).(\<exists> cl'. \<Theta> var = (Some cl') \<and> ground_subcl cl')"
  apply (induction cl rule: substitute_clause.induct)
  apply simp
  using substituted_subcl_ground_substitution_ground
  by (metis Ball_set_list_all image_eqI set_map)

lemma valid_rule_ground_substitution:
"valid_rule r \<Longrightarrow>
 \<forall> cl \<in> Body r. ground_clause (substitute_clause \<Theta> cl) \<Longrightarrow>
 ground_clause (substitute_clause \<Theta> (Head r))"
  unfolding valid_rule_def
  apply (rule ground_substitutions_ground_clause)
  using substituted_clause_ground_substitution_ground
  by force

lemma ground_subcl_inv_unroll:
"ground_subcl cl \<Longrightarrow>
 (\<forall> ucl \<in> set (unroll_subcl cl). ground_clause ucl)"
  apply (induction cl rule:ground_subcl.induct)
     defer
     apply simp
    apply simp
   apply simp
  apply (simp only:ground_subcl.simps(1))
  apply auto
  using split_list_first by fastforce
  

lemma ground_clause_inv_unroll:
"ground_clause cl \<Longrightarrow>
 (\<forall> ucl \<in> set (unroll cl). ground_clause ucl)"
  apply (induction cl rule:ground_clause.induct)
  apply (simp only:ground_clause.simps)
  using ground_subcl_inv_unroll
  by (metis ground_subcl.simps(1) unroll.simps unroll_subcl.simps(1))

lemma ground_db_ICr:
"valid_rule r \<Longrightarrow>
 ground_db db \<Longrightarrow>
 db' = ICr r db \<Longrightarrow> 
 ground_db db'"
  unfolding ground_db_def ICr_def
  using valid_rule_ground_substitution
  using ground_clause_inv_unroll
  by fastforce

lemma ground_db_ICp: 
"valid_program program \<Longrightarrow>
 ground_db db \<Longrightarrow>
 db' = ICp program db \<Longrightarrow> 
 ground_db db'"
  unfolding ICp_def
  unfolding ground_db_def
  using ground_db_ICr
  by auto

definition FixedPointSemantics :: "Program \<Rightarrow> DB" where
"FixedPointSemantics p = lfp (ICp p)"



lemma unroll_subcl_subfact_closed:
"subfact_closed (set (unroll_subcl cl))"
  apply (induction cl rule:unroll_subcl.induct)
     defer
     apply (simp, rule subfact_closed_empty)+
  apply auto
  unfolding subfact_closed_def
  by auto

lemma unroll_subfact_closed:
"subfact_closed (set (unroll cl))"
  apply (induction cl rule:unroll.induct)
  apply (simp only: unroll.simps) 
  using unroll_subcl_subfact_closed
  by (metis unroll_subcl.simps(1)) 

lemma subfact_closed_ICr:
"subfact_closed db \<Longrightarrow>
 subfact_closed (ICr R db)"
  using subfact_closed_union2 subfact_closed_union unroll_subfact_closed
  unfolding ICr_def
  by (smt (verit) mem_Collect_eq)
  
lemma subfact_closed_ICp:
"subfact_closed db \<Longrightarrow>
 subfact_closed (ICp P db)"
  using subfact_closed_union2 subfact_closed_union subfact_closed_ICr
  unfolding ICp_def
  by (smt (verit, ccfv_threshold) mem_Collect_eq)

thm "lfp_ordinal_induct"
thm lfp_ordinal_induct_set
thm "Nat.lfp_Kleene_iter"
thm "fixp_induct"

theorem finite_fp_induct:
"finite (FixedPointSemantics Prog) \<Longrightarrow>
 P {} \<Longrightarrow>
 (\<And> db. P db \<Longrightarrow> P (ICp Prog db)) \<Longrightarrow>
 P (FixedPointSemantics Prog)"
  unfolding FixedPointSemantics_def
  apply (drule Kleene_iter_reaches_finite_lfp[of "ICp Prog", OF mono_ICp])
  apply (subgoal_tac "\<forall> n. P ((ICp Prog ^^ n) {})")
   apply metis
  apply (thin_tac "\<exists>n. (ICp Prog ^^ n) {} = lfp (ICp Prog)")
  apply clarify
  apply (induct_tac n)
   apply simp
  by simp

theorem fp_subfact_closed:
"finite (FixedPointSemantics P) \<Longrightarrow>
 subfact_closed (FixedPointSemantics P)"
  using finite_fp_induct subfact_closed_empty subfact_closed_ICp
  by auto

theorem fp_ground:
"valid_program P \<Longrightarrow>
 finite (FixedPointSemantics P) \<Longrightarrow>
 ground_db (FixedPointSemantics P)"
  using finite_fp_induct ground_db_empty ground_db_ICp
  by blast


section \<open>Model Theoretic Semantics\<close>

definition herbrand_universe :: "Clause set" where
"herbrand_universe \<equiv> {cl | cl:: Clause. ground_clause cl}"

definition "herbrand_interpretation" :: "Clause set \<Rightarrow> bool" where
"herbrand_interpretation I \<equiv> I = \<Union>{set (unroll f)| f. f \<in> I}"

theorem cl_included_in_unroll: "cl \<in> set (unroll cl)"
  apply (induction cl rule:unroll.induct)
  by simp
  
lemma herbrand_interpretation_eq_subfact_closed:
"herbrand_interpretation I = subfact_closed I"
  unfolding herbrand_interpretation_def subfact_closed_def
  apply auto
  apply (subgoal_tac "\<exists>f. f \<in> I \<and> x \<in> set (unroll f)")
   apply auto[1]
  using cl_included_in_unroll
  by auto


definition rule_true_in_interpretation :: "Clause set \<Rightarrow> Rule \<Rightarrow> bool" ("(_) \<Turnstile> (_)" [41] 41) where
"I \<Turnstile> R \<equiv> \<forall> \<Theta>. {cl\<lbrakk>\<Theta>\<rbrakk> | cl . cl \<in> Body R} \<subseteq> I \<longrightarrow> (Head R)\<lbrakk>\<Theta>\<rbrakk> \<in> I" 

definition herbrand_model :: "Clause set \<Rightarrow> Program \<Rightarrow> bool" where
"herbrand_model I P \<equiv> herbrand_interpretation I \<and> (\<forall> R \<in> P. (I \<Turnstile> R))"

definition ModelTheoreticSemantics :: "Program \<Rightarrow> Clause set" where
"ModelTheoreticSemantics P \<equiv> \<Inter> {I. herbrand_model I P}"

lemma herbrand_model_intersect:
"herbrand_model I1 P \<Longrightarrow> herbrand_model I2 P \<Longrightarrow> herbrand_model (I1 \<inter> I2) P"
  unfolding herbrand_model_def
  apply (simp only:herbrand_interpretation_eq_subfact_closed)
  apply auto
   defer
  unfolding rule_true_in_interpretation_def
   apply auto
  by (simp add: subfact_closed_intersection)


lemma herbrand_model_intersect2:
"\<forall> x \<in> S. herbrand_model x P \<Longrightarrow> 
 herbrand_model (\<Inter> S) P"
  unfolding herbrand_model_def
  apply (simp only:herbrand_interpretation_eq_subfact_closed)
  apply auto
   defer
  unfolding rule_true_in_interpretation_def
   apply auto
  using Inter_iff
   apply blast
  using subfact_closed_intersection2 
  by simp

theorem ModelTheoreticSemantics_herband_model:
"herbrand_model (ModelTheoreticSemantics P) P"
  unfolding ModelTheoreticSemantics_def 
  using herbrand_model_intersect2[of _ P]
  by simp


section \<open>Equivalence of Fixed Point and Model Theoretic Semantics\<close>

lemma ICr_includes_db: "db \<subseteq> ICr R db"
  unfolding ICr_def by simp

theorem herbrand_model_is_fp:
  assumes "valid_program P"
      and "herbrand_model I P"
    shows "ICp P I = I"
proof (rule ccontr)
  from \<open>herbrand_model I P\<close> have "subfact_closed I"
    unfolding herbrand_model_def
    using herbrand_interpretation_eq_subfact_closed by auto
  assume "ICp P I \<noteq> I"
  hence "\<exists> R \<in> P. \<not> \<Union> {set (unroll (Head R\<lbrakk>\<Theta>\<rbrakk>)) |\<Theta>. \<forall>b\<in>Body R. b\<lbrakk>\<Theta>\<rbrakk> \<in> I} \<subseteq> I"
    unfolding ICp_def ICr_def by auto
  hence "\<exists> R \<in> P. \<exists> \<Theta>. {cl\<lbrakk>\<Theta>\<rbrakk> | cl. cl \<in> Body R} \<subseteq> I \<and> \<not> set (unroll (Head R\<lbrakk>\<Theta>\<rbrakk>)) \<subseteq> I"
    by auto
  hence "\<exists> R \<in> P. \<exists> \<Theta>. {cl\<lbrakk>\<Theta>\<rbrakk> | cl. cl \<in> Body R} \<subseteq> I \<and> Head R\<lbrakk>\<Theta>\<rbrakk> \<notin> I"
    using \<open>subfact_closed I\<close> subfact_closed_def cl_included_in_unroll by auto
  then obtain R where r:"R \<in> P \<and> (\<exists> \<Theta>. {cl\<lbrakk>\<Theta>\<rbrakk> | cl. cl \<in> Body R} \<subseteq> I \<and> (Head R)\<lbrakk>\<Theta>\<rbrakk> \<notin> I)" by blast
  hence "\<not> I \<Turnstile> R" unfolding rule_true_in_interpretation_def
    by simp
  hence "\<not> (herbrand_model I P)" using r unfolding herbrand_model_def by auto
  thus False using \<open>herbrand_model I P\<close> by auto
qed

theorem fp_is_herbrand_model:
  assumes "valid_program P"
      and "subfact_closed db"
      and "ICp P db = db"
    shows "herbrand_model db P"
proof (rule ccontr)
  assume "\<not> herbrand_model db P"
  hence "\<exists> R \<in> P. \<not> db \<Turnstile> R" unfolding herbrand_model_def
    using \<open>subfact_closed db\<close> herbrand_interpretation_eq_subfact_closed[THEN sym,of "db"]
    by simp
  then obtain R where r:"R \<in> P \<and> \<not> db \<Turnstile> R" by auto
  hence "\<exists> \<Theta>. {cl\<lbrakk>\<Theta>\<rbrakk> | cl . cl \<in> Body R} \<subseteq> db \<and> (Head R)\<lbrakk>\<Theta>\<rbrakk> \<notin> db"
    unfolding rule_true_in_interpretation_def by simp
  hence "\<not> ({(Head R\<lbrakk>\<Theta>\<rbrakk>) |\<Theta>. \<forall>b\<in>Body R. b\<lbrakk>\<Theta>\<rbrakk> \<in> db} \<subseteq> db)" by blast
  hence "\<not> (ICr R db \<subseteq> db)"
    using cl_included_in_unroll unfolding ICr_def by blast
  hence "db \<subset> ICr R db" using ICr_includes_db by auto
  moreover have "R \<in> P" using r by simp
  ultimately have "ICp P db \<noteq> db" unfolding ICp_def by auto
  thus False using \<open>ICp P db = db\<close> by auto
qed

theorem ModelTheoreticSemantics_FixedPointSemantics:
  assumes "valid_program P"
      and "finite (FixedPointSemantics P)"
    shows "ModelTheoreticSemantics P = FixedPointSemantics P"
proof
  have "\<Inter> {fp |fp. fp = ICp P fp } \<subseteq> ModelTheoreticSemantics P"
    using herbrand_model_is_fp \<open>valid_program P\<close>
    using ModelTheoreticSemantics_herband_model by fastforce 
  moreover have "FixedPointSemantics P \<subseteq> \<Inter> {fp. fp = ICp P fp }"
    unfolding FixedPointSemantics_def
    using mono_ICp[of P]
    by (metis (mono_tags, lifting) Inter_greatest lfp_lowerbound mem_Collect_eq order_refl)
  ultimately show "FixedPointSemantics P \<subseteq> ModelTheoreticSemantics P" by auto

  have subfact_closed_fp: "subfact_closed (FixedPointSemantics P)"
    using fp_subfact_closed \<open>valid_program P\<close> \<open>finite (FixedPointSemantics P)\<close> by auto
  hence "herbrand_model (FixedPointSemantics P) P" 
    using fp_is_herbrand_model[OF \<open>valid_program P\<close> subfact_closed_fp]
    unfolding FixedPointSemantics_def
    by (simp add: lfp_fixpoint mono_ICp)
  then show "ModelTheoreticSemantics P \<subseteq> FixedPointSemantics P"
    unfolding ModelTheoreticSemantics_def by auto
qed

end