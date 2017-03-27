defmodule Poison.ParserCsp do
  import Bitwise

  alias Poison.SyntaxError

  @number_start '-0123456789'

  def parse(iodata, options \\ []) do
    string = IO.iodata_to_binary(iodata)
    skip_whitespace(string, 0, fn string, pos ->
      value(string, pos, options[:keys], fn string, pos, value ->
        skip_whitespace(string, pos, fn
          "", _pos -> {:ok, value}
          other, pos -> syntax_error(other, pos)
        end)
      end)
    end)
  catch
    {:invalid, pos} ->
      {:error, :invalid, pos}
    {:invalid, token, pos} ->
      {:error, {:invalid, token, pos}}
  end

  def parse!(iodata, options \\ []) do
    case parse(iodata, options) do
      {:ok, value} ->
        value
      {:error, :invalid, pos} ->
        raise SyntaxError, pos: pos
      {:error, {:invalid, token, pos}} ->
        raise SyntaxError, token: token, pos: pos
    end
  end

  defp value("\"" <> rest, pos, _keys, cont) do
    string_continue(rest, pos + 1, [], cont)
  end

  defp value("{" <> rest, pos, keys, cont) do
    skip_whitespace(rest, pos + 1, fn string, pos ->
      object_pairs(string, pos, keys, [], cont)
    end)
  end

  defp value("[" <> rest, pos, keys, cont) do
    skip_whitespace(rest, pos + 1, fn string, pos ->
      array_values(string, pos, keys, [], cont)
    end)
  end

  defp value("null" <> rest, pos, _keys, cont),  do: cont.(rest, pos + 4, nil)
  defp value("true" <> rest, pos, _keys, cont),  do: cont.(rest, pos + 4, true)
  defp value("false" <> rest, pos, _keys, cont), do: cont.(rest, pos + 5, false)

  defp value(<<char, _ :: binary>> = string, pos, _keys, cont) when char in @number_start do
    number_start(string, pos, cont)
  end

  defp value(other, pos, _keys, _cont), do: syntax_error(other, pos)

  ## Objects

  defp object_pairs("\"" <> rest, pos, keys, acc, cont) do
    string_continue(rest, pos + 1, [], fn string, pos, name ->
      skip_whitespace(string, pos, fn
        ":" <> rest, pos ->
          skip_whitespace(rest, pos + 1, fn string, pos ->
            value(string, pos, keys, fn string, pos, value ->
              acc = [{object_name(name, keys), value} | acc]
              skip_whitespace(string, pos, fn
                "," <> rest, pos ->
                  skip_whitespace(rest, pos + 1, fn string, pos ->
                    object_pairs(string, pos, keys, acc, cont)
                  end)
                "}" <> rest, pos ->
                  cont.(rest, pos + 1, :maps.from_list(acc))
                other, pos ->
                  syntax_error(other, pos)
              end)
            end)
          end)
        other, pos ->
          syntax_error(other, pos)
      end)
    end)
  end

  defp object_pairs("}" <> rest, pos, _keys, [], cont) do
    cont.(rest, pos + 1, %{})
  end

  defp object_pairs(other, pos, _keys, _acc, _cont), do: syntax_error(other, pos)

  defp object_name(name, :atoms),  do: String.to_atom(name)
  defp object_name(name, :atoms!), do: String.to_existing_atom(name)
  defp object_name(name, _keys),   do: name

  ## Arrays

  defp array_values("]" <> rest, pos, _acc, [], cont) do
    cont.(rest, pos + 1, [])
  end

  defp array_values(string, pos, keys, acc, cont) do
    value(string, pos, keys, fn string, pos, value ->
      acc = [value | acc]
      skip_whitespace(string, pos, fn
        "," <> rest, pos ->
          skip_whitespace(rest, pos + 1, fn string, pos ->
            array_values(string, pos, keys, acc, cont)
          end)
        "]" <> rest, pos ->
          cont.(rest, pos, :lists.reverse(acc))
        other, pos ->
          syntax_error(other, pos)
      end)
    end)
  end

  ## Numbers

  defp number_start("-0" <> rest, pos, cont) do
    number_frac(rest, pos + 2, ["-0"], cont)
  end
  defp number_start("-" <> rest, pos, cont) do
    number_int(rest, pos + 1, [?-], cont)
  end
  defp number_start("0" <> rest, pos, cont) do
    number_frac(rest, pos + 1, [?0], cont)
  end
  defp number_start(string, pos, cont) do
    number_int(string, pos, [], cont)
  end

  defp number_int(<<char, _ :: binary>> = string, pos, acc, cont) when char in '123456789' do
    number_digits(string, pos, fn string, pos, digits ->
      number_frac(string, pos, [acc | digits], cont)
    end)
  end
  defp number_int(other, pos, _acc, _cont), do: syntax_error(other, pos)

  defp number_frac("." <> rest, pos, acc, cont) do
    number_digits(rest, pos + 1, fn string, pos, digits ->
      number_exp(string, true, pos, [acc, ?. | digits], cont)
    end)
  end
  defp number_frac(string, pos, acc, cont) do
    number_exp(string, false, pos, acc, cont)
  end

  defp number_exp(<<e>> <> rest, frac, pos, acc, cont) when e in 'eE' do
    e = if frac, do: [?e], else: ".0e"
    case rest do
      "-" <> rest -> number_exp_continue(rest, pos + 2, [acc, e, ?-], cont)
      "+" <> rest -> number_exp_continue(rest, pos + 2, [acc | e], cont)
      rest -> number_exp_continue(rest, pos + 1, [acc | e], cont)
    end
  end
  defp number_exp(string, frac, pos, acc, cont) do
    cont.(string, pos, number_complete(acc, frac))
  end

  defp number_exp_continue(rest, pos, acc, cont) do
    number_digits(rest, pos, fn string, pos, digits ->
      cont.(string, pos, number_complete([acc | digits], true))
    end)
  end

  defp number_complete(iolist, false) do
    iolist |> IO.iodata_to_binary |> String.to_integer
  end
  defp number_complete(iolist, true) do
    iolist |> IO.iodata_to_binary |> String.to_float
  end

  defp number_digits(<<char>> <> rest = string, pos, cont) when char in '0123456789' do
    count = number_digits_count(rest, 1)
    <<digits :: binary-size(count), rest :: binary>> = string
    cont.(rest, pos + count, digits)
  end
  defp number_digits(other, pos, _cont), do: syntax_error(other, pos)

  defp number_digits_count(<<char>> <> rest, acc) when char in '0123456789' do
    number_digits_count(rest, acc+1)
  end
  defp number_digits_count(_, acc), do: acc

  ## Strings

  defp string_continue("\"" <> rest, pos, acc, cont) do
    cont.(rest, pos + 1, IO.iodata_to_binary(acc))
  end

  defp string_continue("\\" <> rest, pos, acc, cont) do
    string_escape(rest, pos, acc, cont)
  end

  defp string_continue("", pos, _, _cont), do: throw({:invalid, pos})

  defp string_continue(string, pos, acc, cont) do
    {count, pos} = string_chunk_size(string, pos, 0)
    <<chunk :: binary-size(count), rest :: binary>> = string
    string_continue(rest, pos, [acc, chunk], cont)
  end

  for {seq, char} <- Enum.zip('"\\ntr/fb', '"\\\n\t\r/\f\b') do
    defp string_escape(<<unquote(seq)>> <> rest, pos, acc, cont) do
      string_continue(rest, pos+1, [acc, unquote(char)], cont)
    end
  end

  # http://www.ietf.org/rfc/rfc2781.txt
  # http://perldoc.perl.org/Encode/Unicode.html#Surrogate-Pairs
  # http://mathiasbynens.be/notes/javascript-encoding#surrogate-pairs
  defp string_escape(<<?u, a1, b1, c1, d1, "\\u", a2, b2, c2, d2>> <> rest, pos, acc, cont)
       when a1 in 'dD' and a2 in 'dD'
            and (b1 in '89abAB')
            and (b2 in ?c..?f or b2 in ?C..?F) do
    hi = List.to_integer([a1, b1, c1, d1], 16)
    lo = List.to_integer([a2, b2, c2, d2], 16)
    codepoint = 0x10000 + ((hi &&& 0x03FF) <<< 10) + (lo &&& 0x03FF)
    string_continue(rest, pos+11, [acc | <<codepoint :: utf8>>], cont)
  end

  defp string_escape(<<?u, seq :: binary-size(4)>> <> rest, pos, acc, cont) do
    string_continue(rest, pos+5, [acc | <<String.to_integer(seq, 16) :: utf8>>], cont)
  end

  defp string_escape(other, pos, _acc, _cont), do: syntax_error(other, pos)

  defp string_chunk_size("\"" <> _, pos, acc), do: {acc, pos}
  defp string_chunk_size("\\" <> _, pos, acc), do: {acc, pos}

  defp string_chunk_size(<<char>> <> rest, pos, acc) when char < 0x80 do
    string_chunk_size(rest, pos+1, acc+1)
  end

  defp string_chunk_size(<<codepoint :: utf8>> <> rest, pos, acc) do
    string_chunk_size(rest, pos+1, acc + string_codepoint_size(codepoint))
  end

  defp string_chunk_size(other, pos, _acc), do: syntax_error(other, pos)

  defp string_codepoint_size(codepoint) when codepoint < 0x800,   do: 2
  defp string_codepoint_size(codepoint) when codepoint < 0x10000, do: 3
  defp string_codepoint_size(_),                                  do: 4

  defp skip_whitespace(<<char>> <> rest, pos, cont) when char in '\s\n\t\r' do
    skip_whitespace(rest, pos+1, cont)
  end

  ## Whitespace

  defp skip_whitespace(string, pos, cont), do: cont.(string, pos)

  ## Errors

  defp syntax_error(<<token :: utf8>> <> _, pos) do
    throw({:invalid, <<token>>, pos})
  end

  defp syntax_error(_, pos) do
    throw({:invalid, pos})
  end
end
