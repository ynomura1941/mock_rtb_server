%% -*- coding: utf-8 -*-

% adid 広告ID
% price 入札金額
% contents 広告html
% domain 広告主のサイトドメイン
% h 高さ
% w 幅
% gender 対象にする性別
% generations 対象にする世代
% self_categories 自身のカテゴリ
% target_categories 対象にするサイトのカテゴリ
% ad_type 広告の種類
-record(rtb_ad, {adid, price, contents, domain, h, w, gender,
        generations, self_categories, target_categories, ad_type, start_datetime, end_datetime}).
