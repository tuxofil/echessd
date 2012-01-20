-module(echessd_html).

-export([table/1, figure/1]).

-include("echessd.hrl").

table(Game) ->
    "<table cellpadding=0 cellspacing=0>\n"
        "<tr>\n"
        "<td>\n"
        "<td class=crd_t><tt>a</tt>\n"
        "<td class=crd_t><tt>b</tt>\n"
        "<td class=crd_t><tt>c</tt>\n"
        "<td class=crd_t><tt>d</tt>\n"
        "<td class=crd_t><tt>e</tt>\n"
        "<td class=crd_t><tt>f</tt>\n"
        "<td class=crd_t><tt>g</tt>\n"
        "<td class=crd_t><tt>h</tt>\n"
        "<td>\n"
        ++ table_rows(Game) ++
        "<tr>\n"
        "<td>\n"
        "<td class=crd_b><tt>a</tt>\n"
        "<td class=crd_b><tt>b</tt>\n"
        "<td class=crd_b><tt>c</tt>\n"
        "<td class=crd_b><tt>d</tt>\n"
        "<td class=crd_b><tt>e</tt>\n"
        "<td class=crd_b><tt>f</tt>\n"
        "<td class=crd_b><tt>g</tt>\n"
        "<td class=crd_b><tt>h</tt>\n"
        "<td>\n"
        "</table>\n".

table_rows(Game) ->
    table_rows(tuple_to_list(Game), 8, []).
table_rows([Row | Tail], N, Result) ->
    StrRow =
        "<tr>\n"
        "<td class=crd_l><tt>" ++ integer_to_list(N) ++ "</tt>\n"
        ++ table_row(Row, N) ++
        "<td class=crd_r><tt>" ++ integer_to_list(N) ++ "</tt>\n",
    table_rows(Tail, N - 1, [StrRow | Result]);
table_rows(_, _, Result) ->
    lists:reverse(Result).

table_row(Row, N) when is_tuple(Row) ->
    table_row(
      tuple_to_list(Row),
      if N rem 2 == 0 -> "wc";
         true -> "bc"
      end);
table_row([Figure | Tail], CellClass) ->
    NewCellClass =
        case CellClass of
            "bc" -> "wc";
            "wc" -> "bc"
        end,
    "<td class=" ++ CellClass ++ ">" ++ figure(Figure) ++ "\n" ++
        table_row(Tail, NewCellClass);
table_row(_, _) -> "".

figure(?empty) -> "&nbsp;";
figure(Fig) ->
    "&#" ++ integer_to_list(figure_(Fig)) ++ ";".
figure_(?w_king  ) -> 9812;
figure_(?w_queen ) -> 9813;
figure_(?w_rook  ) -> 9814;
figure_(?w_bishop) -> 9815;
figure_(?w_knight) -> 9816;
figure_(?w_pawn  ) -> 9817;
figure_(?b_king  ) -> 9818;
figure_(?b_queen ) -> 9819;
figure_(?b_rook  ) -> 9820;
figure_(?b_bishop) -> 9821;
figure_(?b_knight) -> 9822;
figure_(?b_pawn  ) -> 9823.

