---
title: "[lv] Master version controller"
---
A tale I told on IRC about working with git and hg. It's lightly edited for readability and comprehensibility.

**13:25:25 \<ij\>** Preface: sisadmins atsūta taru ar failiem, saka, ka neesot kommitojis šādas tādas izmaiņas. Labi, uztaisu savu gita repo, kodēju.

**13:26:17 \<emilsp\>** ā, un sāls tajā stāstā, ka tars ir labākais kopsavilkums no govnokod.ru?

**13:26:28 \<ij\>** lol

**13:26:32 \<ij\>** Esmu jau kaut kādus desmit kommitus uztaisījis, sisadmins man atsūta pieeju bitbucketa privātajam repo.

**13:26:47 \<emilsp\>** lol

**13:27:16 \<ij\>** Tur ir hg repo ar saturu [html/, src/projectname/\*]. Un ± tas, kas tajā tarā bija iekšā atrodas direktorijā src/projectname.

**13:28:55 \<ij\>** Es nokonvertēju to hg repo par gitu ar fast-export, visiem kommitiem ar rebase uztaisīju "git rm --ignore-unmatch html; git mv -k src/projectname/\* ."

**13:29:13 \<ij\>** Tagad man ir repo, kurš ir salīdzināms ar manu paštaisīto repo.

**13:29:48 \<ij\>** Pa priekšu tara saturu iekopēju tajā konvertētajā repo, nu varēju redzēt diffu visam sisadmina neiekommitotajam; to sadalot, iekommitoju.

**13:30:11 \<ij\>** Tad importēju savu paštaisīto repo kā remoti, fetchoju un varēju cherry-pickot savus kommitus.

**13:30:33 \<ij\>** Un voila, vēsture nav pazaudēta, repo apvienoti un viss ir savās vietās.

**13:32:47 \<emilsp\>** tagad tu esi vcs māsters
