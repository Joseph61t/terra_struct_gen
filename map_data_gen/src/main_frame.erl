-module(main_frame).
-include_lib("wx/include/wx.hrl").
-export([start/0]).
-export([change_size/2,confirm_map/2]).



start() -> %"Terrain and Structure Generation"
   wx:new(),
   % Size_scaler = 5,
   Main_window = wxFrame:new(wx:null(), ?wxID_ANY, "Terrain and Structure Generation", [{pos, {200, 200}}, {size, {500, 400}}]),
   % wxRadioButton:new(Main_window, ?wxID_ANY, "Random Map", [{pos, {5,5}},{size, {415, 20}}]),
   % wxStaticText:new(Main_window, ?wxID_ANY, "Random Map", [{pos, {25,5}},{size, {415, 20}}]),
   wxStaticText:new(Main_window, ?wxID_ANY, "Map Size(units squared):", [{pos, {30,5}},{size, {300, 20}}]),
   Size = wxStaticText:new(Main_window, ?wxID_ANY, integer_to_list(round(math:pow(2,5))), [{pos, {230,6}},{size, {50, 20}}]),
   Size_slide = wxSlider:new(Main_window, ?wxID_ANY, 5, 2, 8, [{style, ?wxSL_LABELS}, {pos, {30,25}},{size, {300, 70}}]), %[{style, ?wxSL_HORIZONTAL bor ?wxSL_TOP}, {pos, {30,40}},{size, {415, 20}}]
   % Size1 = wxStaticText:new(Main_window, ?wxID_ANY, integer_to_list(round(math:pow(2,5))), [{pos, {230,26}},{size, {50, 20}}]),
   % wxStaticText:new(Main_window, ?wxID_ANY, "Map Size(units squared):", [{pos, {30,25}},{size, {300, 20}}]),
   % Size_slide_1 = wxSlider:new(Main_window, ?wxID_ANY, 5, 2, 8, [{style, ?wxSL_LABELS}, {pos, {30,45}},{size, {300, 70}}]), %[{style, ?wxSL_HORIZONTAL bor ?wxSL_TOP}, {pos, {30,40}},{size, {415, 20}}]
   Generate = wxButton:new(Main_window, ?wxID_ANY, [{label, "Generate Map"}, {pos, {5, 361}}, {size, {490, 35}}]),
   % Chosen_generation = wxButton:new(Main_window, ?wxID_ANY, [{label, "Choose Atributes for Map"}, {pos, {5, 361}}, {size, {490, 35}}]),
   % Reset = wxButton:new(Main_window, ?wxID_ANY, [{label, "Reset"}, {pos, {285, 323}}, {size, {135, 75}}]),
   % confirm_size(256,Main_window),
   wxSlider:connect(Size_slide, command_slider_updated, [{callback, fun change_size/2}, {userData, #{size => Size, env => wx:get_env()}}]),
   % wxButton:connect(Random_generation,command_button_clicked, [{callback, fun confirm_map/2},{userData, #{parent => Main_window, size => Size, env => wx:get_env()}}]),
   wxButton:connect(Generate, command_button_clicked, [{callback, fun confirm_map/2},{userData, #{parent => Main_window, size_slider => Size_slide,  env => wx:get_env()}}]),
   % wxSlider:connect(Size_slide,fun(Size1,Size_slide) -> wxStaticText:setLabel(Size1, integer_to_list(round(math:pow(2,wxSlider:getValue(Size_slide)))))end),
   Font = wxFont:new(12, ?wxFONTFAMILY_DEFAULT, ?wxNORMAL, ?wxFONTWEIGHT_NORMAL),
   wxWindow:setFont(Main_window,Font),
   wxFrame:show(Main_window).

change_size(#wx{obj = Size_slider, userData = #{size := Size, env := Env}}, _Event) ->
   wx:set_env(Env),
   wxStaticText:setLabel(Size, integer_to_list(round(math:pow(2,wxSlider:getValue(Size_slider))))).


confirm_map(#wx{obj = Generator, userData = #{parent := Parent, size_slider := Slider, env := Env}}, _Event) ->
% confirm_map(#wx{obj = _Generator, userData = #{parent := Parent, size := Size}}, _Event) ->
   wx:set_env(Env),
   % S_size = integer_to_list(round(math:pow(2,wxSlider:getValue(Slider)))),
   % Question = "Do you want to make a map of size " ++ S_size ++ "X" ++ S_size ++"?",
   % Ask = wxMessageDialog:new(Parent, Question, [{style, 256 bor ?wxYES bor ?wxNO bor ?wxYES_DEFAULT}]),
   % wxMessageDialog:showModal(Ask).
   % io:format("~p~n",[wxSlider:getValue(Slider)]),
   generate_map:random_map(wxSlider:getValue(Slider)).


% https://arifishaq.files.wordpress.com/2017/12/wxerlang-getting-started.pdf