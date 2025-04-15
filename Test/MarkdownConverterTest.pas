unit MarkdownConverterTest;

// c.f. https://github.github.com/gfm/

interface

uses
  System.SysUtils, Rtti,
  DUnitX.TestFramework,
  MyWiki.WikiConverter, MyWiki.MarkdownConverter;

type

  [TestFixture]
  TMarkdownConverterTest = class
  private
    FConverter: TWikiConverter;
    FPageList: TWikiPages;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    // Tabs
    [Test]
    procedure TestWikiToHtmlGmf1;
    [Test]
    procedure TestWikiToHtmlGmf2;
    [Test]
    procedure TestWikiToHtmlGmf3;
    [Test]
    procedure TestWikiToHtmlGmf4;
    [Test]
    procedure TestWikiToHtmlGmf5;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf6;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf7;
    [Test]
    procedure TestWikiToHtmlGmf8;
    [Test]
    procedure TestWikiToHtmlGmf9;
    [Test]
    procedure TestWikiToHtmlGmf10;
    [Test]
    procedure TestWikiToHtmlGmf11;
    [Test]
    procedure TestWikiToHtmlGmf12;

    // Thematic breaks
    [Test]
    procedure TestWikiToHtmlGmf13;
    [Test]
    procedure TestWikiToHtmlGmf14;
    [Test]
    procedure TestWikiToHtmlGmf15;
    [Test]
    procedure TestWikiToHtmlGmf16;
    [Test]
    procedure TestWikiToHtmlGmf17;
    [Test]
    procedure TestWikiToHtmlGmf18;
    [Test]
    procedure TestWikiToHtmlGmf19;
    [Test]
    procedure TestWikiToHtmlGmf20;
    [Test]
    procedure TestWikiToHtmlGmf21;
    [Test]
    procedure TestWikiToHtmlGmf22;
    [Test]
    procedure TestWikiToHtmlGmf23;
    [Test]
    procedure TestWikiToHtmlGmf24;
    [Test]
    procedure TestWikiToHtmlGmf25;
    [Test]
    procedure TestWikiToHtmlGmf26;
    [Test]
    procedure TestWikiToHtmlGmf27;
    [Test]
    procedure TestWikiToHtmlGmf28;
    [Test]
    procedure TestWikiToHtmlGmf29;
    [Test]
    procedure TestWikiToHtmlGmf30;
    [Test]
    procedure TestWikiToHtmlGmf31;

    // ATX headings
    [Test]
    procedure TestWikiToHtmlGmf32;
    [Test]
    procedure TestWikiToHtmlGmf33;
    [Test]
    procedure TestWikiToHtmlGmf34;
    [Test]
    procedure TestWikiToHtmlGmf35;
    [Test]
    procedure TestWikiToHtmlGmf36;
    [Test]
    procedure TestWikiToHtmlGmf37;
    [Test]
    procedure TestWikiToHtmlGmf38;
    [Test]
    procedure TestWikiToHtmlGmf39;
    [Test]
    procedure TestWikiToHtmlGmf40;
    [Test]
    procedure TestWikiToHtmlGmf41;
    [Test]
    procedure TestWikiToHtmlGmf42;
    [Test]
    procedure TestWikiToHtmlGmf43;
    [Test]
    procedure TestWikiToHtmlGmf44;
    [Test]
    procedure TestWikiToHtmlGmf45;
    [Test]
    procedure TestWikiToHtmlGmf46;
    [Test]
    procedure TestWikiToHtmlGmf47;
    [Test]
    procedure TestWikiToHtmlGmf48;
    [Test]
    procedure TestWikiToHtmlGmf49;

    // Setext headings
    [Test]
    procedure TestWikiToHtmlGmf50;
    [Test]
    procedure TestWikiToHtmlGmf51;
    [Test]
    procedure TestWikiToHtmlGmf52;
    [Test]
    procedure TestWikiToHtmlGmf53;
    [Test]
    procedure TestWikiToHtmlGmf54;
    [Test]
    procedure TestWikiToHtmlGmf55;
    [Test]
    procedure TestWikiToHtmlGmf56;
    [Test]
    procedure TestWikiToHtmlGmf57;
    [Test]
    procedure TestWikiToHtmlGmf58;
    [Test]
    procedure TestWikiToHtmlGmf59;
    [Test]
    procedure TestWikiToHtmlGmf60;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf61;
    [Test]
    procedure TestWikiToHtmlGmf62;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf63;
    [Test]
    procedure TestWikiToHtmlGmf64;
    [Test]
    procedure TestWikiToHtmlGmf65;
    [Test]
    procedure TestWikiToHtmlGmf66;
    [Test]
    procedure TestWikiToHtmlGmf67;
    [Test]
    procedure TestWikiToHtmlGmf68;
    [Test]
    procedure TestWikiToHtmlGmf69;
    [Test]
    procedure TestWikiToHtmlGmf70;
    [Test]
    procedure TestWikiToHtmlGmf71;
    [Test]
    procedure TestWikiToHtmlGmf72;
    [Test]
    procedure TestWikiToHtmlGmf73;
    [Test]
    procedure TestWikiToHtmlGmf74;
    [Test]
    procedure TestWikiToHtmlGmf75;
    [Test]
    procedure TestWikiToHtmlGmf76;

    // Indented code blocks
    [Test]
    procedure TestWikiToHtmlGmf77;
    [Test]
    procedure TestWikiToHtmlGmf77_2; // additional test
    [Test]
    procedure TestWikiToHtmlGmf77_3; // additional test
    [Test]
    procedure TestWikiToHtmlGmf78;
    [Test]
    procedure TestWikiToHtmlGmf79;
    [Test]
    procedure TestWikiToHtmlGmf80;
    [Test]
    procedure TestWikiToHtmlGmf81;
    [Test]
    procedure TestWikiToHtmlGmf82;
    [Test]
    procedure TestWikiToHtmlGmf83;
    [Test]
    procedure TestWikiToHtmlGmf84;
    [Test]
    procedure TestWikiToHtmlGmf85;
    [Test]
    procedure TestWikiToHtmlGmf86;
    [Test]
    procedure TestWikiToHtmlGmf87;
    [Test]
    procedure TestWikiToHtmlGmf88;

    // Fenced code blocks
    [Test]
    procedure TestWikiToHtmlGmf89;
    [Test]
    procedure TestWikiToHtmlGmf90;
    [Test]
    procedure TestWikiToHtmlGmf91;
    [Test]
    procedure TestWikiToHtmlGmf92;
    [Test]
    procedure TestWikiToHtmlGmf93;
    [Test]
    procedure TestWikiToHtmlGmf94;
    [Test]
    procedure TestWikiToHtmlGmf95;
    [Test]
    procedure TestWikiToHtmlGmf96;
    [Test]
    procedure TestWikiToHtmlGmf97;
    [Test]
    procedure TestWikiToHtmlGmf98;
    [Test]
    procedure TestWikiToHtmlGmf99;
    [Test]
    procedure TestWikiToHtmlGmf100;
    [Test]
    procedure TestWikiToHtmlGmf101;
    [Test]
    procedure TestWikiToHtmlGmf102;
    [Test]
    procedure TestWikiToHtmlGmf103;
    [Test]
    procedure TestWikiToHtmlGmf104;
    [Test]
    procedure TestWikiToHtmlGmf105;
    [Test]
    procedure TestWikiToHtmlGmf106;
    [Test]
    procedure TestWikiToHtmlGmf107;
    [Test]
    procedure TestWikiToHtmlGmf108;
    [Test]
    procedure TestWikiToHtmlGmf109;
    [Test]
    procedure TestWikiToHtmlGmf110;
    [Test]
    procedure TestWikiToHtmlGmf111;
    [Test]
    procedure TestWikiToHtmlGmf112;
    [Test]
    procedure TestWikiToHtmlGmf113;
    [Test]
    procedure TestWikiToHtmlGmf114;
    [Test]
    procedure TestWikiToHtmlGmf115;
    [Test]
    procedure TestWikiToHtmlGmf116;
    [Test]
    procedure TestWikiToHtmlGmf117;

    // HTML blocks
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf118;
    [Test]
    procedure TestWikiToHtmlGmf119;
    [Test]
    procedure TestWikiToHtmlGmf120;
    [Test]
    procedure TestWikiToHtmlGmf121;
    [Test]
    procedure TestWikiToHtmlGmf122;
    [Test]
    procedure TestWikiToHtmlGmf123;
    [Test]
    procedure TestWikiToHtmlGmf124;
    [Test]
    procedure TestWikiToHtmlGmf125;
    [Test]
    procedure TestWikiToHtmlGmf126;
    [Test]
    procedure TestWikiToHtmlGmf127;
    [Test]
    procedure TestWikiToHtmlGmf128;
    [Test]
    procedure TestWikiToHtmlGmf129;
    [Test]
    procedure TestWikiToHtmlGmf130;
    [Test]
    procedure TestWikiToHtmlGmf131;
    [Test]
    procedure TestWikiToHtmlGmf132;
    [Test]
    procedure TestWikiToHtmlGmf133;
    [Test]
    procedure TestWikiToHtmlGmf134;
    [Test]
    procedure TestWikiToHtmlGmf135;
    [Test]
    procedure TestWikiToHtmlGmf136;
    [Test]
    procedure TestWikiToHtmlGmf137;
    [Test]
    procedure TestWikiToHtmlGmf138;
    [Test]
    procedure TestWikiToHtmlGmf139;
    [Test]
    procedure TestWikiToHtmlGmf140;
    [Test]
    procedure TestWikiToHtmlGmf141;
    [Test]
    procedure TestWikiToHtmlGmf142;
    [Test]
    procedure TestWikiToHtmlGmf143;
    [Test]
    procedure TestWikiToHtmlGmf144;
    [Test]
    procedure TestWikiToHtmlGmf145;
    [Test]
    procedure TestWikiToHtmlGmf146;
    [Test]
    procedure TestWikiToHtmlGmf147;
    [Test]
    procedure TestWikiToHtmlGmf148;
    [Test]
    procedure TestWikiToHtmlGmf149;
    [Test]
    procedure TestWikiToHtmlGmf150;
    [Test]
    procedure TestWikiToHtmlGmf151;
    [Test]
    procedure TestWikiToHtmlGmf152;
    [Test]
    procedure TestWikiToHtmlGmf153;
    [Test]
    procedure TestWikiToHtmlGmf154;
    [Test]
    procedure TestWikiToHtmlGmf155;
    [Test]
    procedure TestWikiToHtmlGmf156;
    [Test]
    procedure TestWikiToHtmlGmf157;
    [Test]
    procedure TestWikiToHtmlGmf158;
    [Test]
    procedure TestWikiToHtmlGmf159;
    [Test]
    procedure TestWikiToHtmlGmf160;

    // Link reference definitions
    [Test]
    procedure TestWikiToHtmlGmf161;
    [Test]
    procedure TestWikiToHtmlGmf161_2; // additional test
    [Test]
    procedure TestWikiToHtmlGmf162;
    [Test]
    procedure TestWikiToHtmlGmf163;
    [Test]
    procedure TestWikiToHtmlGmf164;
    [Test]
    procedure TestWikiToHtmlGmf165;
    [Test]
    procedure TestWikiToHtmlGmf166;
    [Test]
    procedure TestWikiToHtmlGmf167;
    [Test]
    procedure TestWikiToHtmlGmf168;
    [Test]
    procedure TestWikiToHtmlGmf169;
    [Test]
    procedure TestWikiToHtmlGmf170;
    [Test]
    procedure TestWikiToHtmlGmf171;
    [Test]
    procedure TestWikiToHtmlGmf172;
    [Test]
    procedure TestWikiToHtmlGmf173;
    [Test]
    procedure TestWikiToHtmlGmf174;
    [Test]
    procedure TestWikiToHtmlGmf175;
    [Test]
    procedure TestWikiToHtmlGmf176;
    [Test]
    procedure TestWikiToHtmlGmf177;
    [Test]
    procedure TestWikiToHtmlGmf178;
    [Test]
    procedure TestWikiToHtmlGmf179;
    [Test]
    procedure TestWikiToHtmlGmf180;
    [Test]
    procedure TestWikiToHtmlGmf181;
    [Test]
    procedure TestWikiToHtmlGmf182;
    [Test]
    procedure TestWikiToHtmlGmf183;
    [Test]
    procedure TestWikiToHtmlGmf184;
    [Test]
    procedure TestWikiToHtmlGmf185;
    [Test]
    procedure TestWikiToHtmlGmf186;
    [Test]
    procedure TestWikiToHtmlGmf187;
    [Test]
    procedure TestWikiToHtmlGmf188;

    // Paragraphs
    [Test]
    procedure TestWikiToHtmlGmf189;
    [Test]
    procedure TestWikiToHtmlGmf190;
    [Test]
    procedure TestWikiToHtmlGmf191;
    [Test]
    procedure TestWikiToHtmlGmf192;
    [Test]
    procedure TestWikiToHtmlGmf193;
    [Test]
    procedure TestWikiToHtmlGmf194;
    [Test]
    procedure TestWikiToHtmlGmf195;
    [Test]
    procedure TestWikiToHtmlGmf196;

    // Blank lines
    [Test]
    procedure TestWikiToHtmlGmf197;

    // Tables
    [Test]
    procedure TestWikiToHtmlGmf198;
    [Test]
    procedure TestWikiToHtmlGmf198_2; // additional test
    [Test]
    procedure TestWikiToHtmlGmf199;
    [Test]
    procedure TestWikiToHtmlGmf200;
    [Test]
    procedure TestWikiToHtmlGmf201;
    [Test]
    procedure TestWikiToHtmlGmf201_2; // additional test
    [Test]
    procedure TestWikiToHtmlGmf201_3; // additional test
    [Test]
    procedure TestWikiToHtmlGmf202;
    [Test]
    procedure TestWikiToHtmlGmf203;
    [Test]
    procedure TestWikiToHtmlGmf204;
    [Test]
    procedure TestWikiToHtmlGmf205;

    // Block quotes
    [Test]
    procedure TestWikiToHtmlGmf206;
    [Test]
    procedure TestWikiToHtmlGmf207;
    [Test]
    procedure TestWikiToHtmlGmf208;
    [Test]
    procedure TestWikiToHtmlGmf209;
    [Test]
    procedure TestWikiToHtmlGmf210;
    [Test]
    procedure TestWikiToHtmlGmf211;
    [Test]
    procedure TestWikiToHtmlGmf212;
    [Test]
    procedure TestWikiToHtmlGmf213;
    [Test]
    procedure TestWikiToHtmlGmf214;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf215;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf216;
    [Test]
    procedure TestWikiToHtmlGmf217;
    [Test]
    procedure TestWikiToHtmlGmf218;
    [Test]
    procedure TestWikiToHtmlGmf219;
    [Test]
    procedure TestWikiToHtmlGmf220;
    [Test]
    procedure TestWikiToHtmlGmf221;
    [Test]
    procedure TestWikiToHtmlGmf222;
    [Test]
    procedure TestWikiToHtmlGmf223;
    [Test]
    procedure TestWikiToHtmlGmf224;
    [Test]
    procedure TestWikiToHtmlGmf225;
    [Test]
    procedure TestWikiToHtmlGmf226;
    [Test]
    procedure TestWikiToHtmlGmf227;
    [Test]
    procedure TestWikiToHtmlGmf228;
    [Test]
    procedure TestWikiToHtmlGmf229;
    [Test]
    procedure TestWikiToHtmlGmf230;

    // List items
    [Test]
    procedure TestWikiToHtmlGmf231;
    [Test]
    procedure TestWikiToHtmlGmf232;
    [Test]
    procedure TestWikiToHtmlGmf232_2; // additional test
    [Test]
    procedure TestWikiToHtmlGmf233;
    [Test]
    procedure TestWikiToHtmlGmf234;
    [Test]
    procedure TestWikiToHtmlGmf235;
    [Test]
    procedure TestWikiToHtmlGmf236;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf237;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf238;
    [Test]
    procedure TestWikiToHtmlGmf239;
    [Test]
    procedure TestWikiToHtmlGmf240;
    [Test]
    procedure TestWikiToHtmlGmf241;
    [Test]
    procedure TestWikiToHtmlGmf242;
    [Test]
    procedure TestWikiToHtmlGmf243;
    [Test]
    procedure TestWikiToHtmlGmf244;
    [Test]
    procedure TestWikiToHtmlGmf245;
    [Test]
    procedure TestWikiToHtmlGmf246;
    [Test]
    procedure TestWikiToHtmlGmf247;
    [Test]
    procedure TestWikiToHtmlGmf248;
    [Test]
    procedure TestWikiToHtmlGmf249;
    [Test]
    procedure TestWikiToHtmlGmf250;
    [Test]
    procedure TestWikiToHtmlGmf251;
    [Test]
    procedure TestWikiToHtmlGmf252;
    [Test]
    procedure TestWikiToHtmlGmf253;
    [Test]
    procedure TestWikiToHtmlGmf254;
    [Test]
    procedure TestWikiToHtmlGmf255;
    [Test]
    procedure TestWikiToHtmlGmf256;
    [Test]
    procedure TestWikiToHtmlGmf256_2; // additional test
    [Test]
    procedure TestWikiToHtmlGmf257;
    [Test]
    procedure TestWikiToHtmlGmf258;
    [Test]
    procedure TestWikiToHtmlGmf259;
    [Test]
    procedure TestWikiToHtmlGmf260;
    [Test]
    procedure TestWikiToHtmlGmf261;
    [Test]
    procedure TestWikiToHtmlGmf262;
    [Test]
    procedure TestWikiToHtmlGmf262_2; // additional test
    [Test]
    procedure TestWikiToHtmlGmf263;
    [Test]
    procedure TestWikiToHtmlGmf264;
    [Test]
    procedure TestWikiToHtmlGmf265;
    [Test]
    procedure TestWikiToHtmlGmf266;
    [Test]
    procedure TestWikiToHtmlGmf267;
    [Test]
    procedure TestWikiToHtmlGmf268;
    [Test]
    procedure TestWikiToHtmlGmf269;
    [Test]
    procedure TestWikiToHtmlGmf270;
    [Test]
    procedure TestWikiToHtmlGmf271;
    [Test]
    procedure TestWikiToHtmlGmf272;
    [Test]
    procedure TestWikiToHtmlGmf273;
    [Test]
    procedure TestWikiToHtmlGmf274;
    [Test]
    procedure TestWikiToHtmlGmf275;
    [Test]
    procedure TestWikiToHtmlGmf276;
    [Test]
    procedure TestWikiToHtmlGmf277;
    [Test]
    procedure TestWikiToHtmlGmf278;

    // Task list items
    [Test]
    procedure TestWikiToHtmlGmf279;
    [Test]
    procedure TestWikiToHtmlGmf280;

    // Lists
    [Test]
    procedure TestWikiToHtmlGmf281;
    [Test]
    procedure TestWikiToHtmlGmf282;
    [Test]
    procedure TestWikiToHtmlGmf283;
    [Test]
    procedure TestWikiToHtmlGmf284;
    [Test]
    procedure TestWikiToHtmlGmf285;
    [Test]
    procedure TestWikiToHtmlGmf286;
    [Test]
    procedure TestWikiToHtmlGmf286_2; // additional test
    [Test]
    procedure TestWikiToHtmlGmf287;
    [Test]
    procedure TestWikiToHtmlGmf287_2; // additional test
    [Test]
    procedure TestWikiToHtmlGmf288;
    [Test]
    procedure TestWikiToHtmlGmf289;
    [Test]
    procedure TestWikiToHtmlGmf290;
    [Test]
    procedure TestWikiToHtmlGmf291;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf292;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf293;
    [Test]
    procedure TestWikiToHtmlGmf294;
    [Test]
    procedure TestWikiToHtmlGmf295;
    [Test]
    procedure TestWikiToHtmlGmf296;
    [Test]
    procedure TestWikiToHtmlGmf297;
    [Test]
    procedure TestWikiToHtmlGmf298;
    [Test]
    procedure TestWikiToHtmlGmf299;
    [Test]
    procedure TestWikiToHtmlGmf300;
    [Test]
    procedure TestWikiToHtmlGmf301;
    [Test]
    procedure TestWikiToHtmlGmf302;
    [Test]
    procedure TestWikiToHtmlGmf303;
    [Test]
    procedure TestWikiToHtmlGmf304;
    [Test]
    procedure TestWikiToHtmlGmf305;
    [Test]
    procedure TestWikiToHtmlGmf306;
    [Test]
    procedure TestWikiToHtmlNestedList; // additional test

    // Inlines
    [Test]
    procedure TestWikiToHtmlGmf307;

    // Backslash escapes
    [Test]
    procedure TestWikiToHtmlGmf308;
    [Test]
    procedure TestWikiToHtmlGmf309;
    [Test]
    procedure TestWikiToHtmlGmf310;
    [Test]
    procedure TestWikiToHtmlGmf311;
    [Test]
    procedure TestWikiToHtmlGmf312;
    [Test]
    procedure TestWikiToHtmlGmf313;
    [Test]
    procedure TestWikiToHtmlGmf314;
    [Test]
    procedure TestWikiToHtmlGmf315;
    [Test]
    procedure TestWikiToHtmlGmf316;
    [Test]
    procedure TestWikiToHtmlGmf317;
    [Test]
    procedure TestWikiToHtmlGmf318;
    [Test]
    procedure TestWikiToHtmlGmf319;
    [Test]
    procedure TestWikiToHtmlGmf320;

    // Entity and numeric character references
    [Test]
    procedure TestWikiToHtmlGmf321;
    [Test]
    procedure TestWikiToHtmlGmf322;
    [Test]
    procedure TestWikiToHtmlGmf323;
    [Test]
    procedure TestWikiToHtmlGmf324;
    [Test]
    procedure TestWikiToHtmlGmf325;
    [Test]
    procedure TestWikiToHtmlGmf326;
    [Test]
    procedure TestWikiToHtmlGmf327;
    [Test]
    procedure TestWikiToHtmlGmf328;
    [Test]
    procedure TestWikiToHtmlGmf329;
    [Test]
    procedure TestWikiToHtmlGmf330;
    [Test]
    procedure TestWikiToHtmlGmf331;
    [Test]
    procedure TestWikiToHtmlGmf332;
    [Test]
    procedure TestWikiToHtmlGmf333;
    [Test]
    procedure TestWikiToHtmlGmf334;
    [Test]
    procedure TestWikiToHtmlGmf335;
    [Test]
    procedure TestWikiToHtmlGmf336;
    [Test]
    procedure TestWikiToHtmlGmf337;

    // Code spans
    [Test]
    procedure TestWikiToHtmlGmf338;
    [Test]
    procedure TestWikiToHtmlGmf339;
    [Test]
    procedure TestWikiToHtmlGmf340;
    [Test]
    procedure TestWikiToHtmlGmf341;
    [Test]
    procedure TestWikiToHtmlGmf342;
    [Test]
    procedure TestWikiToHtmlGmf343;
    [Test]
    procedure TestWikiToHtmlGmf344;
    [Test]
    procedure TestWikiToHtmlGmf345;
    [Test]
    procedure TestWikiToHtmlGmf346;
    [Test]
    procedure TestWikiToHtmlGmf347;
    [Test]
    procedure TestWikiToHtmlGmf348;
    [Test]
    procedure TestWikiToHtmlGmf349;
    [Test]
    procedure TestWikiToHtmlGmf350;
    [Test]
    procedure TestWikiToHtmlGmf351;
    [Test]
    procedure TestWikiToHtmlGmf352;
    [Test]
    procedure TestWikiToHtmlGmf353;
    [Test]
    procedure TestWikiToHtmlGmf354;
    [Test]
    procedure TestWikiToHtmlGmf355;
    [Test]
    procedure TestWikiToHtmlGmf356;
    [Test]
    procedure TestWikiToHtmlGmf357;
    [Test]
    procedure TestWikiToHtmlGmf358;
    [Test]
    procedure TestWikiToHtmlGmf359;

    // Emphasis and strong emphasis
    [Test]
    procedure TestWikiToHtmlGmf360;
    [Test]
    procedure TestWikiToHtmlGmf361;
    [Test]
    procedure TestWikiToHtmlGmf362;
    [Test]
    procedure TestWikiToHtmlGmf363;
    [Test]
    procedure TestWikiToHtmlGmf364;
    [Test]
    procedure TestWikiToHtmlGmf365;
    [Test]
    procedure TestWikiToHtmlGmf366;
    [Test]
    procedure TestWikiToHtmlGmf367;
    [Test]
    procedure TestWikiToHtmlGmf368;
    [Test]
    procedure TestWikiToHtmlGmf369;
    [Test]
    procedure TestWikiToHtmlGmf370;
    [Test]
    procedure TestWikiToHtmlGmf371;
    [Test]
    procedure TestWikiToHtmlGmf372;
    [Test]
    procedure TestWikiToHtmlGmf373;
    [Test]
    procedure TestWikiToHtmlGmf374;
    [Test]
    procedure TestWikiToHtmlGmf375;
    [Test]
    procedure TestWikiToHtmlGmf376;
    [Test]
    procedure TestWikiToHtmlGmf377;
    [Test]
    procedure TestWikiToHtmlGmf378;
    [Test]
    procedure TestWikiToHtmlGmf379;
    [Test]
    procedure TestWikiToHtmlGmf380;
    [Test]
    procedure TestWikiToHtmlGmf381;
    [Test]
    procedure TestWikiToHtmlGmf382;
    [Test]
    procedure TestWikiToHtmlGmf383;
    [Test]
    procedure TestWikiToHtmlGmf384;
    [Test]
    procedure TestWikiToHtmlGmf385;
    [Test]
    procedure TestWikiToHtmlGmf386;
    [Test]
    procedure TestWikiToHtmlGmf387;
    [Test]
    procedure TestWikiToHtmlGmf388;
    [Test]
    procedure TestWikiToHtmlGmf389;
    [Test]
    procedure TestWikiToHtmlGmf390;
    [Test]
    procedure TestWikiToHtmlGmf391;
    [Test]
    procedure TestWikiToHtmlGmf392;
    [Test]
    procedure TestWikiToHtmlGmf393;
    [Test]
    procedure TestWikiToHtmlGmf394;
    [Test]
    procedure TestWikiToHtmlGmf395;
    [Test]
    procedure TestWikiToHtmlGmf396;
    [Test]
    procedure TestWikiToHtmlGmf397;
    [Test]
    procedure TestWikiToHtmlGmf398;
    [Test]
    procedure TestWikiToHtmlGmf399;
    [Test]
    procedure TestWikiToHtmlGmf400;
    [Test]
    procedure TestWikiToHtmlGmf401;
    [Test]
    procedure TestWikiToHtmlGmf402;
    [Test]
    procedure TestWikiToHtmlGmf403;
    [Test]
    procedure TestWikiToHtmlGmf404;
    [Test]
    procedure TestWikiToHtmlGmf405;
    [Test]
    procedure TestWikiToHtmlGmf406;
    [Test]
    procedure TestWikiToHtmlGmf407;
    [Test]
    procedure TestWikiToHtmlGmf408;
    [Test]
    procedure TestWikiToHtmlGmf409;
    [Test]
    procedure TestWikiToHtmlGmf410;
    [Test]
    procedure TestWikiToHtmlGmf411;
    [Test]
    procedure TestWikiToHtmlGmf412;
    [Test]
    procedure TestWikiToHtmlGmf413;
    [Test]
    procedure TestWikiToHtmlGmf414;
    [Test]
    procedure TestWikiToHtmlGmf415;
    [Test]
    procedure TestWikiToHtmlGmf416;
    [Test]
    procedure TestWikiToHtmlGmf417;
    [Test]
    procedure TestWikiToHtmlGmf418;
    [Test]
    procedure TestWikiToHtmlGmf419;
    [Test]
    procedure TestWikiToHtmlGmf420;
    [Test]
    procedure TestWikiToHtmlGmf421;
    [Test]
    procedure TestWikiToHtmlGmf422;
    [Test]
    procedure TestWikiToHtmlGmf423;
    [Test]
    procedure TestWikiToHtmlGmf424;
    [Test]
    procedure TestWikiToHtmlGmf425;
    [Test]
    procedure TestWikiToHtmlGmf426;
    [Test]
    procedure TestWikiToHtmlGmf427;
    [Test]
    procedure TestWikiToHtmlGmf428;
    [Test]
    procedure TestWikiToHtmlGmf429;
    [Test]
    procedure TestWikiToHtmlGmf430;
    [Test]
    procedure TestWikiToHtmlGmf431;
    [Test]
    procedure TestWikiToHtmlGmf432;
    [Test]
    procedure TestWikiToHtmlGmf433;
    [Test]
    procedure TestWikiToHtmlGmf434;
    [Test]
    procedure TestWikiToHtmlGmf435;
    [Test]
    procedure TestWikiToHtmlGmf436;
    [Test]
    procedure TestWikiToHtmlGmf437;
    [Test]
    procedure TestWikiToHtmlGmf438;
    [Test]
    procedure TestWikiToHtmlGmf439;
    [Test]
    procedure TestWikiToHtmlGmf440;
    [Test]
    procedure TestWikiToHtmlGmf441;
    [Test]
    procedure TestWikiToHtmlGmf442;
    [Test]
    procedure TestWikiToHtmlGmf443;
    [Test]
    procedure TestWikiToHtmlGmf444;
    [Test]
    procedure TestWikiToHtmlGmf445;
    [Test]
    procedure TestWikiToHtmlGmf446;
    [Test]
    procedure TestWikiToHtmlGmf447;
    [Test]
    procedure TestWikiToHtmlGmf448;
    [Test]
    procedure TestWikiToHtmlGmf449;
    [Test]
    procedure TestWikiToHtmlGmf450;
    [Test]
    procedure TestWikiToHtmlGmf451;
    [Test]
    procedure TestWikiToHtmlGmf452;
    [Test]
    procedure TestWikiToHtmlGmf453;
    [Test]
    procedure TestWikiToHtmlGmf454;
    [Test]
    procedure TestWikiToHtmlGmf455;
    [Test]
    procedure TestWikiToHtmlGmf456;
    [Test]
    procedure TestWikiToHtmlGmf457;
    [Test]
    procedure TestWikiToHtmlGmf458;
    [Test]
    procedure TestWikiToHtmlGmf459;
    [Test]
    procedure TestWikiToHtmlGmf460;
    [Test]
    procedure TestWikiToHtmlGmf461;
    [Test]
    procedure TestWikiToHtmlGmf462;
    [Test]
    procedure TestWikiToHtmlGmf463;
    [Test]
    procedure TestWikiToHtmlGmf464;
    [Test]
    procedure TestWikiToHtmlGmf465;
    [Test]
    procedure TestWikiToHtmlGmf466;
    [Test]
    procedure TestWikiToHtmlGmf467;
    [Test]
    procedure TestWikiToHtmlGmf468;
    [Test]
    procedure TestWikiToHtmlGmf469;
    [Test]
    procedure TestWikiToHtmlGmf470;
    [Test]
    procedure TestWikiToHtmlGmf471;
    [Test]
    procedure TestWikiToHtmlGmf472;
    [Test]
    procedure TestWikiToHtmlGmf473;
    [Test]
    procedure TestWikiToHtmlGmf474;
    [Test]
    procedure TestWikiToHtmlGmf475;
    [Test]
    procedure TestWikiToHtmlGmf476;
    [Test]
    procedure TestWikiToHtmlGmf477;
    [Test]
    procedure TestWikiToHtmlGmf478;
    [Test]
    procedure TestWikiToHtmlGmf479;
    [Test]
    procedure TestWikiToHtmlGmf480;
    [Test]
    procedure TestWikiToHtmlGmf481;
    [Test]
    procedure TestWikiToHtmlGmf482;
    [Test]
    procedure TestWikiToHtmlGmf483;
    [Test]
    procedure TestWikiToHtmlGmf484;
    [Test]
    procedure TestWikiToHtmlGmf485;
    [Test]
    procedure TestWikiToHtmlGmf486;
    [Test]
    procedure TestWikiToHtmlGmf487;
    [Test]
    procedure TestWikiToHtmlGmf488;
    [Test]
    procedure TestWikiToHtmlGmf489;
    [Test]
    procedure TestWikiToHtmlGmf490;

    // Strikethrough
    [Test]
    procedure TestWikiToHtmlGmf491;
    [Test]
    procedure TestWikiToHtmlGmf492;
    [Test]
    procedure TestWikiToHtmlGmf493;

    // Links
    [Test]
    procedure TestWikiToHtmlGmf494;
    [Test]
    procedure TestWikiToHtmlGmf495;
    [Test]
    procedure TestWikiToHtmlGmf496;
    [Test]
    procedure TestWikiToHtmlGmf497;
    [Test]
    procedure TestWikiToHtmlGmf498;
    [Test]
    procedure TestWikiToHtmlGmf499;
    [Test]
    procedure TestWikiToHtmlGmf500;
    [Test]
    procedure TestWikiToHtmlGmf501;
    [Test]
    procedure TestWikiToHtmlGmf502;
    [Test]
    procedure TestWikiToHtmlGmf503;
    [Test]
    procedure TestWikiToHtmlGmf504;
    [Test]
    procedure TestWikiToHtmlGmf505;
    [Test]
    procedure TestWikiToHtmlGmf506;
    [Test]
    procedure TestWikiToHtmlGmf507;
    [Test]
    procedure TestWikiToHtmlGmf508;
    [Test]
    procedure TestWikiToHtmlGmf509;
    [Test]
    procedure TestWikiToHtmlGmf510;
    [Test]
    procedure TestWikiToHtmlGmf511;
    [Test]
    procedure TestWikiToHtmlGmf512;
    [Test]
    procedure TestWikiToHtmlGmf513;
    [Test]
    procedure TestWikiToHtmlGmf514;
    [Test]
    procedure TestWikiToHtmlGmf515;
    [Test]
    procedure TestWikiToHtmlGmf516;
    [Test]
    procedure TestWikiToHtmlGmf517;
    [Test]
    procedure TestWikiToHtmlGmf518;
    [Test]
    procedure TestWikiToHtmlGmf519;
    [Test]
    procedure TestWikiToHtmlGmf520;
    [Test]
    procedure TestWikiToHtmlGmf521;
    [Test]
    procedure TestWikiToHtmlGmf522;
    [Test]
    procedure TestWikiToHtmlGmf523;
    [Test]
    procedure TestWikiToHtmlGmf524;
    [Test]
    procedure TestWikiToHtmlGmf525;
    [Test]
    procedure TestWikiToHtmlGmf526;
    [Test]
    procedure TestWikiToHtmlGmf527;
    [Test]
    procedure TestWikiToHtmlGmf527_2; // additional test
    [Test]
    procedure TestWikiToHtmlGmf528;
    [Test]
    procedure TestWikiToHtmlGmf529;
    [Test]
    procedure TestWikiToHtmlGmf530;
    [Test]
    procedure TestWikiToHtmlGmf531;
    [Test]
    procedure TestWikiToHtmlGmf532;
    [Test]
    procedure TestWikiToHtmlGmf533;
    [Test]
    procedure TestWikiToHtmlGmf534;
    [Test]
    procedure TestWikiToHtmlGmf535;
    [Test]
    procedure TestWikiToHtmlGmf536;
    [Test]
    procedure TestWikiToHtmlGmf537;
    [Test]
    procedure TestWikiToHtmlGmf538;
    [Test]
    procedure TestWikiToHtmlGmf539;
    [Test]
    procedure TestWikiToHtmlGmf540;
    [Test]
    procedure TestWikiToHtmlGmf541;
    [Test]
    procedure TestWikiToHtmlGmf542;
    [Test]
    procedure TestWikiToHtmlGmf543;
    [Test]
    procedure TestWikiToHtmlGmf544;
    [Test]
    procedure TestWikiToHtmlGmf545;
    [Test]
    procedure TestWikiToHtmlGmf546;
    [Test]
    procedure TestWikiToHtmlGmf547;
    [Test]
    procedure TestWikiToHtmlGmf548;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf549;
    [Test]
    procedure TestWikiToHtmlGmf550;
    [Test]
    procedure TestWikiToHtmlGmf550_2; // additional test
    [Test]
    procedure TestWikiToHtmlGmf551;
    [Test]
    procedure TestWikiToHtmlGmf552;
    [Test]
    procedure TestWikiToHtmlGmf553;
    [Test]
    procedure TestWikiToHtmlGmf554;
    [Test]
    procedure TestWikiToHtmlGmf555;
    [Test]
    procedure TestWikiToHtmlGmf556;
    [Test]
    procedure TestWikiToHtmlGmf557;
    [Test]
    procedure TestWikiToHtmlGmf558;
    [Test]
    procedure TestWikiToHtmlGmf559;
    [Test]
    procedure TestWikiToHtmlGmf560;
    [Test]
    procedure TestWikiToHtmlGmf561;
    [Test]
    procedure TestWikiToHtmlGmf562;
    [Test]
    procedure TestWikiToHtmlGmf563;
    [Test]
    procedure TestWikiToHtmlGmf564;
    [Test]
    procedure TestWikiToHtmlGmf565;
    [Test]
    procedure TestWikiToHtmlGmf566;
    [Test]
    procedure TestWikiToHtmlGmf567;
    [Test]
    procedure TestWikiToHtmlGmf568;
    [Test]
    procedure TestWikiToHtmlGmf569;
    [Test]
    procedure TestWikiToHtmlGmf570;
    [Test]
    procedure TestWikiToHtmlGmf571;
    [Test]
    procedure TestWikiToHtmlGmf572;
    [Test]
    procedure TestWikiToHtmlGmf573;
    [Test]
    procedure TestWikiToHtmlGmf574;
    [Test]
    procedure TestWikiToHtmlGmf575;
    [Test]
    procedure TestWikiToHtmlGmf576;
    [Test]
    procedure TestWikiToHtmlGmf577;
    [Test]
    procedure TestWikiToHtmlGmf578;
    [Test]
    procedure TestWikiToHtmlGmf579;
    [Test]
    procedure TestWikiToHtmlGmf580;

    // Images
    [Test]
    procedure TestWikiToHtmlGmf581;
    [Test]
    procedure TestWikiToHtmlGmf582;
    [Test]
    procedure TestWikiToHtmlGmf583;
    [Test]
    procedure TestWikiToHtmlGmf584;
    [Test]
    procedure TestWikiToHtmlGmf585;
    [Test]
    procedure TestWikiToHtmlGmf586;
    [Test]
    procedure TestWikiToHtmlGmf587;
    [Test]
    procedure TestWikiToHtmlGmf588;
    [Test]
    procedure TestWikiToHtmlGmf589;
    [Test]
    procedure TestWikiToHtmlGmf590;
    [Test]
    procedure TestWikiToHtmlGmf591;
    [Test]
    procedure TestWikiToHtmlGmf592;
    [Test]
    procedure TestWikiToHtmlGmf593;
    [Test]
    procedure TestWikiToHtmlGmf594;
    [Test]
    procedure TestWikiToHtmlGmf595;
    [Test]
    procedure TestWikiToHtmlGmf596;
    [Test]
    procedure TestWikiToHtmlGmf597;
    [Test]
    procedure TestWikiToHtmlGmf598;
    [Test]
    procedure TestWikiToHtmlGmf599;
    [Test]
    procedure TestWikiToHtmlGmf600;
    [Test]
    procedure TestWikiToHtmlGmf601;
    [Test]
    procedure TestWikiToHtmlGmf602;

    // Autolinks
    [Test]
    procedure TestWikiToHtmlGmf603;
    [Test]
    procedure TestWikiToHtmlGmf604;
    [Test]
    procedure TestWikiToHtmlGmf605;
    [Test]
    procedure TestWikiToHtmlGmf606;
    [Test]
    procedure TestWikiToHtmlGmf607;
    [Test]
    procedure TestWikiToHtmlGmf608;
    [Test]
    procedure TestWikiToHtmlGmf609;
    [Test]
    procedure TestWikiToHtmlGmf610;
    [Test]
    procedure TestWikiToHtmlGmf611;
    [Test]
    procedure TestWikiToHtmlGmf612;
    [Test]
    procedure TestWikiToHtmlGmf613;
    [Test]
    procedure TestWikiToHtmlGmf614;
    [Test]
    procedure TestWikiToHtmlGmf615;
    [Test]
    procedure TestWikiToHtmlGmf616;
    [Test]
    procedure TestWikiToHtmlGmf617;
    [Test]
    procedure TestWikiToHtmlGmf618;
    [Test]
    procedure TestWikiToHtmlGmf619;
    [Test]
    procedure TestWikiToHtmlGmf620;
    [Test]
    procedure TestWikiToHtmlGmf621;

    // Autolinks extension is not supported (622-628, 630-635)
    [Test]
    procedure TestWikiToHtmlGmf629;
    [Test]
    procedure TestWikiToHtmlGmf629_2; // additional test

    // Raw HTML
    [Test]
    procedure TestWikiToHtmlGmf636;
    [Test]
    procedure TestWikiToHtmlGmf637;
    [Test]
    procedure TestWikiToHtmlGmf638;
    [Test]
    procedure TestWikiToHtmlGmf639;
    [Test]
    procedure TestWikiToHtmlGmf640;
    [Test]
    procedure TestWikiToHtmlGmf641;
    [Test]
    procedure TestWikiToHtmlGmf642;
    [Test]
    procedure TestWikiToHtmlGmf643;
    [Test]
    procedure TestWikiToHtmlGmf644;
    [Test]
    procedure TestWikiToHtmlGmf645;
    [Test]
    procedure TestWikiToHtmlGmf646;
    [Test]
    procedure TestWikiToHtmlGmf647;
    [Test]
    procedure TestWikiToHtmlGmf648;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf649;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf650;
    [Test]
    procedure TestWikiToHtmlGmf651;
    [Test]
    procedure TestWikiToHtmlGmf652;
    [Test]
    procedure TestWikiToHtmlGmf653;
    [Test]
    procedure TestWikiToHtmlGmf654;
    [Test]
    procedure TestWikiToHtmlGmf655;
    [Test]
    procedure TestWikiToHtmlGmf656;
    [Test]
    [Ignore('Not compatible')]
    procedure TestWikiToHtmlGmf657;

    // Hard line breaks
    [Test]
    procedure TestWikiToHtmlGmf658;
    [Test]
    procedure TestWikiToHtmlGmf659;
    [Test]
    procedure TestWikiToHtmlGmf660;
    [Test]
    procedure TestWikiToHtmlGmf661;
    [Test]
    procedure TestWikiToHtmlGmf662;

    [Test]
    procedure TestWikiToHtmlGmf669;
    [Test]
    procedure TestWikiToHtmlGmf670;
    [Test]
    procedure TestWikiToHtmlGmf671;
    [Test]
    procedure TestWikiToHtmlGmf672;

    // Soft line breaks
    [Test]
    procedure TestWikiToHtmlGmf673;
    [Test]
    procedure TestWikiToHtmlGmf674;

    // Textual content
    [Test]
    procedure TestWikiToHtmlGmf675;
    [Test]
    procedure TestWikiToHtmlGmf676;
    [Test]
    procedure TestWikiToHtmlGmf677;

    // page name
    [Test]
    procedure TestPageName;
    [Test]
    procedure TestPageName2;
    [Test]
    procedure TestPageName3;
    [Test]
    procedure TestPageName4;
    [Test]
    procedure TestPageName5;

    // footnote
    [Test]
    procedure TestFootnote;
    [Test]
    procedure TestFootnote2;
    [Test]
    procedure TestFootnote3;
    [Test]
    procedure TestFootnote4;
    [Test]
    procedure TestFootnote5;
    [Test]
    procedure TestFootnote6;
    [Test]
    procedure TestFootnote7;

    // add ID to Header
    [Test]
    procedure TestAddIdToHeader;
    [Test]
    procedure TestAddIdToHeader2;
    [Test]
    procedure TestAddIdToHeader3;
    [Test]
    procedure TestAddIdToHeader4;
    [Test]
    procedure TestAddIdToHeader5;
    [Test]
    procedure TestAddIdToHeader6;

  end;

implementation

procedure TMarkdownConverterTest.Setup;
begin
  FConverter := TMarkdownConverter.Create;
  FPageList := TWikiPages.Create;

  // set page
  FPageList.Add('Test Page', 'https://TestPage');
  FConverter.SetPageList(FPageList);
end;

procedure TMarkdownConverterTest.TearDown;
begin
  FConverter.Free;
  FPageList.Free;
end;

procedure TMarkdownConverterTest.TestAddIdToHeader;
var
  Expected: String;
  Sentence: String;
begin
  FConverter.GiveIdToHeader := True;
  Sentence := String.Join(WikiLB, ['# foo', '# bar']);
  Expected := String.Join(WikiLB, ['<h1 id="foo">foo</h1>',
    '<h1 id="bar">bar</h1>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestAddIdToHeader2;
var
  Expected: String;
  Sentence: String;
begin
  FConverter.GiveIdToHeader := True;
  Sentence := String.Join(WikiLB, ['# foo', '# foo']);
  Expected := String.Join(WikiLB, ['<h1 id="foo">foo</h1>',
    '<h1 id="foo-1">foo</h1>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestAddIdToHeader3;
var
  Expected: String;
  Sentence: String;
begin
  FConverter.GiveIdToHeader := True;
  Sentence := String.Join(WikiLB, ['# foo-bar', '# foo_bar', '# foo+bar',
    '# foo/bar']);
  Expected := String.Join(WikiLB, ['<h1 id="foo-bar">foo-bar</h1>',
    '<h1 id="foo_bar">foo_bar</h1>', '<h1 id="foobar">foo+bar</h1>',
    '<h1 id="foobar-1">foo/bar</h1>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestAddIdToHeader4;
var
  Expected: String;
  Sentence: String;
begin
  FConverter.GiveIdToHeader := True;
  Sentence := String.Join(WikiLB, ['# foo', '# foo', '# foo-1', '# bar-1']);
  Expected := String.Join(WikiLB, ['<h1 id="foo">foo</h1>',
    '<h1 id="foo-1">foo</h1>', '<h1 id="foo-1-1">foo-1</h1>',
    '<h1 id="bar-1">bar-1</h1>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestAddIdToHeader5;
var
  Expected: String;
  Sentence: String;
begin
  FConverter.GiveIdToHeader := True;
  Sentence := String.Join(WikiLB, ['# foo bar', '# foo-bar']);
  Expected := String.Join(WikiLB, ['<h1 id="foo-bar">foo bar</h1>',
    '<h1 id="foo-bar-1">foo-bar</h1>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestAddIdToHeader6;
var
  Expected: String;
  Sentence: String;
begin
  FConverter.GiveIdToHeader := True;
  Sentence := String.Join(WikiLB, ['foo', '===']);
  Expected := String.Join(WikiLB, ['<h1 id="foo">foo</h1>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestFootnote;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['test[^ft] test', '', '[^ft]:footnote']);
  Expected := String.Join(WikiLB,
    ['<p>test<a id="fnref-ft" class="fnref" href="#fn-ft">[1]</a> test</p>',
    '<section class="footnotes">', '<ol>', '<li id="fn-ft">footnote',
    '<a class="fnarrow" href="#fnref-ft">↩</a></li>', '</ol>', '</section>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestFootnote2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['test[^ft]', '', 'test2[^ft2]', '',
    '[^ft]:footnote', '[^ft2]:footnote2']);
  Expected := String.Join(WikiLB,
    ['<p>test<a id="fnref-ft" class="fnref" href="#fn-ft">[1]</a></p>',
    '<p>test2<a id="fnref-ft2" class="fnref" href="#fn-ft2">[2]</a></p>',
    '<section class="footnotes">', '<ol>', '<li id="fn-ft">footnote',
    '<a class="fnarrow" href="#fnref-ft">↩</a></li>',
    '<li id="fn-ft2">footnote2',
    '<a class="fnarrow" href="#fnref-ft2">↩</a></li>', '</ol>', '</section>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestFootnote3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['test[^ft]', '', '[^ft]:**footnote**']);
  Expected := String.Join(WikiLB,
    ['<p>test<a id="fnref-ft" class="fnref" href="#fn-ft">[1]</a></p>',
    '<section class="footnotes">', '<ol>',
    '<li id="fn-ft"><strong>footnote</strong>',
    '<a class="fnarrow" href="#fnref-ft">↩</a></li>', '</ol>', '</section>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestFootnote4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['test[^ft]', '', '[^ft]:footnote[^ft2]',
    '[^ft2]:footnote2']);
  Expected := String.Join(WikiLB,
    ['<p>test<a id="fnref-ft" class="fnref" href="#fn-ft">[1]</a></p>',
    '<section class="footnotes">', '<ol>',
    '<li id="fn-ft">footnote<a id="fnref-ft2" class="fnref" href="#fn-ft2">[2]</a>',
    '<a class="fnarrow" href="#fnref-ft">↩</a></li>',
    '<li id="fn-ft2">footnote2',
    '<a class="fnarrow" href="#fnref-ft2">↩</a></li>', '</ol>', '</section>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestFootnote5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['test[^ft]', '', 'test2[^ft2]', '',
    '[^ft2]:footnote2', '[^ft]:footnote']);
  Expected := String.Join(WikiLB,
    ['<p>test<a id="fnref-ft" class="fnref" href="#fn-ft">[1]</a></p>',
    '<p>test2<a id="fnref-ft2" class="fnref" href="#fn-ft2">[2]</a></p>',
    '<section class="footnotes">', '<ol>', '<li id="fn-ft">footnote',
    '<a class="fnarrow" href="#fnref-ft">↩</a></li>',
    '<li id="fn-ft2">footnote2',
    '<a class="fnarrow" href="#fnref-ft2">↩</a></li>', '</ol>', '</section>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestFootnote6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['test[^ft]', '', 'test2[^ft]', '',
    '[^ft]:footnote']);
  Expected := String.Join(WikiLB,
    ['<p>test<a id="fnref-ft" class="fnref" href="#fn-ft">[1]</a></p>',
    '<p>test2<a id="fnref-ft-2" class="fnref" href="#fn-ft">[1]</a></p>',
    '<section class="footnotes">', '<ol>', '<li id="fn-ft">footnote',
    '<a class="fnarrow" href="#fnref-ft">↩</a>',
    '<a class="fnarrow" href="#fnref-ft-2">↩<sup>2</sup></a></li>', '</ol>',
    '</section>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestFootnote7;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['test[^ft]', '', '[^ft]:footnote',
    '  2nd line', '', '  4th line.', '', '', 'next paragraph']);
  Expected := String.Join(WikiLB,
    ['<p>test<a id="fnref-ft" class="fnref" href="#fn-ft">[1]</a></p>',
    '<p>next paragraph</p>', '<section class="footnotes">', '<ol>',
    '<li id="fn-ft">footnote<br />', '2nd line<br />', '<br />', '4th line.',
    '<a class="fnarrow" href="#fnref-ft">↩</a></li>', '</ol>', '</section>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestPageName;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[[Test Page]]';
  Expected := '<p><a href="https://TestPage">Test Page</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestPageName2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[[Test Page2]]';
  Expected := '<p>[[Test Page2]]</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestPageName3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[[Alias|Test Page]]';
  Expected := '<p><a href="https://TestPage">Alias</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestPageName4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[[Alias|Test Page2]]';
  Expected := '<p>[[Alias|Test Page2]]</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestPageName5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[[Alias|https://www.example.com]]';
  Expected := '<p><a href="https://www.example.com">Alias</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf1;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [#9'foo'#9'baz'#9#9'bim']);
  Expected := String.Join(WikiLB, ['<pre><code>foo'#9'baz'#9#9'bim',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf10;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '#'#9'Foo';
  Expected := '<h1>Foo</h1>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf100;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['```', '```']);
  Expected := '<pre><code></code></pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf101;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' ```', ' aaa', 'aaa', '```']);
  Expected := String.Join(WikiLB, ['<pre><code>aaa', 'aaa', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf102;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['  ```', 'aaa', '  aaa', 'aaa', '  ```']);
  Expected := String.Join(WikiLB, ['<pre><code>aaa', 'aaa', 'aaa',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf103;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['   ```', '   aaa', '    aaa', '  aaa',
    '   ```']);
  Expected := String.Join(WikiLB, ['<pre><code>aaa', ' aaa', 'aaa',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf104;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['    ```', '    aaa', '    ```']);
  Expected := String.Join(WikiLB, ['<pre><code>```', 'aaa', '```',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf105;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['```', 'aaa', '  ```']);
  Expected := String.Join(WikiLB, ['<pre><code>aaa', '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf106;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['   ```', 'aaa', '  ```']);
  Expected := String.Join(WikiLB, ['<pre><code>aaa', '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf107;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['```', 'aaa', '    ```']);
  Expected := String.Join(WikiLB, ['<pre><code>aaa', '    ```', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf108;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['``` ```', 'aaa']);
  Expected := String.Join(WikiLB, ['<p><code> </code>', 'aaa</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf109;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['~~~~~~', 'aaa', '~~~ ~~']);
  Expected := String.Join(WikiLB, ['<pre><code>aaa', '~~~ ~~', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf11;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*'#9'*'#9'*'#9;
  Expected := '<hr />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf110;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['foo', '```', 'bar', '```', 'baz']);
  Expected := String.Join(WikiLB, ['<p>foo</p>', '<pre><code>bar',
    '</code></pre>', '<p>baz</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf111;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['foo', '---', '~~~', 'bar', '~~~', '# baz']);
  Expected := String.Join(WikiLB, ['<h2>foo</h2>', '<pre><code>bar',
    '</code></pre>', '<h1>baz</h1>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf112;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['```ruby', 'def foo(x)', '  return 3',
    'end', '```']);
  Expected := String.Join(WikiLB,
    ['<pre><code class="language-ruby">def foo(x)', '  return 3', 'end',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf113;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['~~~~    ruby startline=3 $%@#$',
    'def foo(x)', '  return 3', 'end', '~~~~~~~']);
  Expected := String.Join(WikiLB,
    ['<pre><code class="language-ruby">def foo(x)', '  return 3', 'end',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf114;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['````;', '````']);
  Expected := '<pre><code class="language-;"></code></pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf115;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['``` aa ```', 'foo']);
  Expected := String.Join(WikiLB, ['<p><code>aa</code>', 'foo</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf116;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['~~~ aa ``` ~~~', 'foo', '~~~']);
  Expected := String.Join(WikiLB, ['<pre><code class="language-aa">foo',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf117;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['```', '``` aaa', '```']);
  Expected := String.Join(WikiLB, ['<pre><code>``` aaa', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf118;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<table><tr><td>', '<pre>', '**Hello**,', '',
    '_world_.', '</pre>', '</td></tr></table>']);
  Expected := String.Join(WikiLB, ['<table><tr><td>', '<pre>', '**Hello**,',
    '<p><em>world</em>.', '</pre></p>', '</td></tr></table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf119;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<table>', '  <tr>', '    <td>',
    '           hi', '    </td>', '  </tr>', '</table>', '', 'okay.']);
  Expected := String.Join(WikiLB, ['<table>', '  <tr>', '    <td>',
    '           hi', '    </td>', '  </tr>', '</table>', '<p>okay.</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf12;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- `one', '- two`']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>`one</li>', '<li>two`</li>',
    '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf120;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' <div>', '  *hello*', '         <foo><a>']);
  Expected := String.Join(WikiLB, [' <div>', '  *hello*', '         <foo><a>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf121;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['</div>', '*foo*']);
  Expected := String.Join(WikiLB, ['</div>', '*foo*']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf122;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<DIV CLASS="foo">', '', '*Markdown*', '',
    '</DIV>']);
  Expected := String.Join(WikiLB, ['<DIV CLASS="foo">',
    '<p><em>Markdown</em></p>', '</DIV>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf123;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<div id="foo"', '  class="bar">',
    '</div>']);
  Expected := String.Join(WikiLB, ['<div id="foo"', '  class="bar">', '</div>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf124;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<div id="foo" class="bar', '  baz">',
    '</div>']);
  Expected := String.Join(WikiLB, ['<div id="foo" class="bar', '  baz">',
    '</div>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf125;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<div>', '*foo*', '', '*bar*']);
  Expected := String.Join(WikiLB, ['<div>', '*foo*', '<p><em>bar</em></p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf126;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<div id="foo"', '*hi*']);
  Expected := String.Join(WikiLB, ['<div id="foo"', '*hi*']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf127;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<div class', 'foo']);
  Expected := String.Join(WikiLB, ['<div class', 'foo']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf128;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<div *???-&&&-<---', '*foo*']);
  Expected := String.Join(WikiLB, ['<div *???-&&&-<---', '*foo*']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf129;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<div><a href="bar">*foo*</a></div>';
  Expected := '<div><a href="bar">*foo*</a></div>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf13;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '***' + WikiLB + '---' + WikiLB + '___';
  Expected := '<hr />' + WikiLB + '<hr />' + WikiLB + '<hr />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf130;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<table><tr><td>', 'foo',
    '</td></tr></table>']);
  Expected := String.Join(WikiLB, ['<table><tr><td>', 'foo',
    '</td></tr></table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf131;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<div></div>', '``` c',
    'int x = 33;', '```']);
  Expected := String.Join(WikiLB, ['<div></div>', '``` c', 'int x = 33;', '```']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf132;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<a href="foo">', '*bar*', '</a>']);
  Expected := String.Join(WikiLB, ['<a href="foo">', '*bar*', '</a>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf133;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<Warning>', '*bar*', '</Warning>']);
  Expected := String.Join(WikiLB, ['<Warning>', '*bar*', '</Warning>'])
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf134;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<i class="foo">', '*bar*', '</i>']);
  Expected := String.Join(WikiLB, ['<i class="foo">', '*bar*', '</i>'])
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf135;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['</ins>', '*bar*']);
  Expected := String.Join(WikiLB, ['</ins>', '*bar*']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf136;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<del>', '*foo*', '</del>']);
  Expected := String.Join(WikiLB, ['<del>', '*foo*', '</del>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf137;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<del>', '', '*foo*', '', '</del>']);
  Expected := String.Join(WikiLB, ['<del>', '<p><em>foo</em></p>', '</del>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf138;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<del>*foo*</del>';
  Expected := '<p><del><em>foo</em></del></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf139;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<pre language="haskell"><code>',
    'import Text.HTML.TagSoup', '', 'main :: IO ()',
    'main = print $ parseTags tags', '</code></pre>', 'okay']);
  Expected := String.Join(WikiLB, ['<pre language="haskell"><code>',
    'import Text.HTML.TagSoup', '', 'main :: IO ()',
    'main = print $ parseTags tags', '</code></pre>', '<p>okay</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf14;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '+++';
  Expected := '<p>+++</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf140;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<script type="text/javascript">',
    '// JavaScript example', '',
    'document.getElementById("demo").innerHTML = "Hello JavaScript!";',
    '</script>', 'okay']);
  Expected := String.Join(WikiLB, ['<script type="text/javascript">',
    '// JavaScript example', '',
    'document.getElementById("demo").innerHTML = "Hello JavaScript!";',
    '</script>', '<p>okay</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf141;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<style', '  type="text/css">',
    'h1 {color:red;}', '', 'p {color:blue;}', '</style>', 'okay']);
  Expected := String.Join(WikiLB, ['<style', '  type="text/css">',
    'h1 {color:red;}', '', 'p {color:blue;}', '</style>', '<p>okay</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf142;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<style', '  type="text/css">', '', 'foo']);
  Expected := String.Join(WikiLB, ['<style', '  type="text/css">', '', 'foo']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf143;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> <div>', '> foo', '', 'bar']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<div>', 'foo',
    '</blockquote>', '<p>bar</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf144;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- <div>', '- foo']);
  // NOTE: The position of line breaks is arranged.
  Expected := String.Join(WikiLB, ['<ul>', '<li><div>', '</li>', '<li>foo</li>',
    '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf145;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<style>p{color:red;}</style>', '*foo*']);
  Expected := String.Join(WikiLB, ['<style>p{color:red;}</style>',
    '<p><em>foo</em></p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf146;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<!-- foo -->*bar*', '*baz*']);
  Expected := String.Join(WikiLB, ['<!-- foo -->*bar*', '<p><em>baz</em></p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf147;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<script>', 'foo', '</script>1. *bar*']);
  Expected := String.Join(WikiLB, ['<script>', 'foo', '</script>1. *bar*']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf148;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<!-- Foo', '', 'bar', '   baz -->',
    'okay']);
  Expected := String.Join(WikiLB, ['<!-- Foo', '', 'bar', '   baz -->',
    '<p>okay</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf149;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<?php', '', '  echo ' > ';', '',
    '?>', 'okay']);
  Expected := String.Join(WikiLB, ['<?php', '', '  echo ' > ';', '', '?>',
    '<p>okay</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf15;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '===';
  Expected := '<p>===</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf150;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<!DOCTYPE html>';
  Expected := '<!DOCTYPE html>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf151;
var
  Expected: String;
  Sentence: String;
  Lines: array of string;
begin
  Lines := ['<![CDATA[', 'function matchwo(a,b)', '{',
    '  if (a < b && a < 0) then {', '    return 1;', '', '  } else {', '',
    '    return 0;', '  }', '}', ']]>', 'okay'];
  Sentence := String.Join(WikiLB, Lines);

  Lines := ['<![CDATA[', 'function matchwo(a,b)', '{',
    '  if (a < b && a < 0) then {', '    return 1;', '', '  } else {', '',
    '    return 0;', '  }', '}', ']]>', '<p>okay</p>'];
  Expected := String.Join(WikiLB, Lines) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf152;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['  <!-- foo -->', '', '    <!-- foo -->']);
  Expected := String.Join(WikiLB, ['  <!-- foo -->',
    '<pre><code>&lt;!-- foo --&gt;', '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf153;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['  <div>', '', '    <div>']);
  Expected := String.Join(WikiLB, ['  <div>', '<pre><code>&lt;div&gt;',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf154;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', '<div>', 'bar', '</div>']);
  Expected := String.Join(WikiLB, ['<p>Foo</p>', '<div>', 'bar', '</div>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf155;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<div>', 'bar', '</div>', '*foo*']);
  Expected := String.Join(WikiLB, ['<div>', 'bar', '</div>', '*foo*']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf156;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', '<a href="bar">', 'baz']);
  Expected := String.Join(WikiLB, ['<p>Foo', '<a href="bar">', 'baz</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf157;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<div>', '', '*Emphasized* text.', '',
    '</div>']);
  Expected := String.Join(WikiLB, ['<div>', '<p><em>Emphasized</em> text.</p>',
    '</div>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf158;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<div>', '*Emphasized* text.', '</div>']);
  Expected := String.Join(WikiLB, ['<div>', '*Emphasized* text.', '</div>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf159;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<table>', '', '<tr>', '', '<td>', 'Hi',
    '</td>', '', '</tr>', '', '</table>']);
  Expected := String.Join(WikiLB, ['<table>', '<tr>', '<td>', 'Hi', '</td>',
    '</tr>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf16;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '--' + WikiLB + '**' + WikiLB + '__';
  Expected := '<p>--' + WikiLB + '**' + WikiLB + '__</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf160;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<table>', '', '  <tr>', '', '    <td>',
    '      Hi', '    </td>', '', '  </tr>', '', '</table>']);
  Expected := String.Join(WikiLB, ['<table>', '  <tr>', '<pre><code>&lt;td&gt;',
    '  Hi', '&lt;/td&gt;', '</code></pre>', '  </tr>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf161;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]: /url "title"', '', '[foo]']);
  Expected := '<p><a href="/url" title="title">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));

  // check definition clear
  Sentence := String.Join(WikiLB, ['[foo]: /url2 "title2"', '', '[foo]']);
  Expected := '<p><a href="/url2" title="title2">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf161_2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]: /url "title"', '', '[foo]',
    '[foo]']);
  Expected := String.Join(WikiLB, ['<p><a href="/url" title="title">foo</a>',
    '<a href="/url" title="title">foo</a></p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf162;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['   [foo]: ', '      /url  ',
    '           ''the title''  ', '', '[foo]']);
  Expected := '<p><a href="/url" title="the title">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf163;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB,
    ['[Foo*bar\]]:my_(url) ''title (with parens)''', '', '[Foo*bar\]]']);
  Expected :=
    '<p><a href="my_(url)" title="title (with parens)">Foo*bar]</a></p>' +
    WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf164;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[Foo bar]:', '<my url>', '''title''', '',
    '[Foo bar]']);
  Expected := '<p><a href="my%20url" title="title">Foo bar</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf165;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[foo]: /url ''' + WikiLB + 'title' + WikiLB + 'line1' + WikiLB +
    'line2' + WikiLB + '''' + WikiLB + '' + WikiLB + '[foo]';
  Expected := String.Join(WikiLB, ['<p><a href="/url" title="', 'title',
    'line1', 'line2', '">foo</a></p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf166;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]: /url ''title', '',
    'with blank line''', '', '[foo]']);
  Expected := String.Join(WikiLB, ['<p>[foo]: /url ''title</p>',
    '<p>with blank line''</p>', '<p>[foo]</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf167;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]:', '/url', '', '[foo]']);
  Expected := '<p><a href="/url">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf168;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]:', '', '[foo]']);
  Expected := String.Join(WikiLB, ['<p>[foo]:</p>', '<p>[foo]</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf169;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]: <>', '', '[foo]']);
  Expected := '<p><a href="">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf17;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ' ***' + WikiLB + '  ***' + WikiLB + '   ***';
  Expected := '<hr />' + WikiLB + '<hr />' + WikiLB + '<hr />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf170;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]: <bar>(baz)', '', '[foo]']);
  Expected := String.Join(WikiLB, ['<p>[foo]: <bar>(baz)</p>', '<p>[foo]</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf171;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]: /url\bar\*baz "foo\"bar\baz"', '',
    '[foo]']);
  Expected :=
    '<p><a href="/url%5Cbar*baz" title="foo&quot;bar\baz">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf172;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]', '', '[foo]: url']);
  Expected := '<p><a href="url">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf173;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]', '', '[foo]: first',
    '[foo]: second']);
  Expected := '<p><a href="first">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf174;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[FOO]: /url', '', '[Foo]']);
  Expected := '<p><a href="/url">Foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf175;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[ΑΓΩ]: /φου', '', '[αγω]']);
  Expected := '<p><a href="/%CF%86%CE%BF%CF%85">αγω</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf176;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[foo]: /url';
  Expected := '';
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf177;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[' + WikiLB + 'foo' + WikiLB + ']: /url' + WikiLB + 'bar';
  Expected := '<p>bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf178;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[foo]: /url "title" ok';
  Expected := '<p>[foo]: /url &quot;title&quot; ok</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf179;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]: /url', '"title" ok']);
  Expected := '<p>&quot;title&quot; ok</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf18;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '    ***';
  Expected := '<pre><code>***' + WikiLB + '</code></pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf180;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['    [foo]: /url "title"', '', '[foo]']);
  Expected := String.Join(WikiLB, ['<pre><code>[foo]: /url &quot;title&quot;',
    '</code></pre>', '<p>[foo]</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf181;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['```', '[foo]: /url', '```', '', '[foo]']);
  Expected := String.Join(WikiLB, ['<pre><code>[foo]: /url', '</code></pre>',
    '<p>[foo]</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf182;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', '[bar]: /baz', '', '[bar]']);
  Expected := String.Join(WikiLB, ['<p>Foo', '[bar]: /baz</p>', '<p>[bar]</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf183;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['# [Foo]', '[foo]: /url', '> bar']);
  Expected := String.Join(WikiLB, ['<h1><a href="/url">Foo</a></h1>',
    '<blockquote>', '<p>bar</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf184;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]: /url', 'bar', '===', '[foo]']);
  Expected := String.Join(WikiLB, ['<h1>bar</h1>',
    '<p><a href="/url">foo</a></p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf185;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]: /url', '===', '[foo]']);
  Expected := String.Join(WikiLB, ['<p>===', '<a href="/url">foo</a></p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf186;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]: /foo-url "foo"', '[bar]: /bar-url',
    '  "bar"', '[baz]: /baz-url', '', '[foo],', '[bar],', '[baz]']);
  Expected := String.Join(WikiLB, ['<p><a href="/foo-url" title="foo">foo</a>,',
    '<a href="/bar-url" title="bar">bar</a>,', '<a href="/baz-url">baz</a></p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf187;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]', '', '> [foo]: /url']);
  Expected := String.Join(WikiLB, ['<p><a href="/url">foo</a></p>',
    '<blockquote>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf188;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[foo]: /url';
  Expected := '';
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf189;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'aaa' + WikiLB + WikiLB + 'bbb';
  Expected := '<p>aaa</p>' + WikiLB + '<p>bbb</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf19;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'Foo' + WikiLB + '    ***';
  Expected := '<p>Foo' + WikiLB + '***</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf190;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'aaa' + WikiLB + 'bbb' + WikiLB + WikiLB + 'ccc' + WikiLB + 'ddd';
  Expected := '<p>aaa' + WikiLB + 'bbb</p>' + WikiLB + '<p>ccc' + WikiLB +
    'ddd</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf191;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'aaa' + WikiLB + WikiLB + WikiLB + WikiLB + 'bbb';
  Expected := '<p>aaa</p>' + WikiLB + '<p>bbb</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf192;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '  aaa' + WikiLB + ' bbb';
  Expected := '<p>aaa' + WikiLB + 'bbb</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf193;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'aaa' + WikiLB + '             bbb' + WikiLB +
    '                                       ccc';
  Expected := '<p>aaa' + WikiLB + 'bbb' + WikiLB + 'ccc</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf194;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '   aaa' + WikiLB + 'bbb';
  Expected := '<p>aaa' + WikiLB + 'bbb</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf195;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '    aaa' + WikiLB + 'bbb';
  Expected := '<pre><code>aaa' + WikiLB + '</code></pre>' + WikiLB +
    '<p>bbb</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf196;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := WikiLB + 'aaa     ' + WikiLB + 'bbb     ';
  Expected := '<p>aaa<br />' + WikiLB + 'bbb</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf197;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := WikiLB + 'aaa' + WikiLB + WikiLB + '# aaa' + WikiLB + WikiLB;
  Expected := '<p>aaa</p>' + WikiLB + '<h1>aaa</h1>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf198;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| foo | bar |', '| --- | --- |',
    '| baz | bim |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>', '<th>foo</th>',
    '<th>bar</th>', '</tr>', '</thead>', '<tbody>', '<tr>', '<td>baz</td>',
    '<td>bim</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf198_2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| foo | bar |', '| --- | --- |',
    '| baz | bim |', '', '| foo | bar |', '| --- | --- |', '| baz | bim |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>', '<th>foo</th>',
    '<th>bar</th>', '</tr>', '</thead>', '<tbody>', '<tr>', '<td>baz</td>',
    '<td>bim</td>', '</tr>', '</tbody>', '</table>', '<table>', '<thead>',
    '<tr>', '<th>foo</th>', '<th>bar</th>', '</tr>', '</thead>', '<tbody>',
    '<tr>', '<td>baz</td>', '<td>bim</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf199;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| abc | defghi |', ':-: | -----------:',
    'bar | baz']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th align="center">abc</th>', '<th align="right">defghi</th>', '</tr>',
    '</thead>', '<tbody>', '<tr>', '<td align="center">bar</td>',
    '<td align="right">baz</td>', '</tr>', '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['  '#9'foo'#9'baz'#9#9'bim']);
  Expected := String.Join(WikiLB, ['<pre><code>foo'#9'baz'#9#9'bim',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf20;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_____________________________________';
  Expected := '<hr />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf200;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| f\|oo  |', '| ------ |', '| b `\|` az |',
    '| b **\|** im |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>',
    '<th>f|oo</th>', '</tr>', '</thead>', '<tbody>', '<tr>',
    '<td>b <code>|</code> az</td>', '</tr>', '<tr>',
    '<td>b <strong>|</strong> im</td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf201;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| abc | def |', '| --- | --- |',
    '| bar | baz |', '> bar']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>', '<th>abc</th>',
    '<th>def</th>', '</tr>', '</thead>', '<tbody>', '<tr>', '<td>bar</td>',
    '<td>baz</td>', '</tr>', '</tbody>', '</table>', '<blockquote>',
    '<p>bar</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf201_2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| abc | def | ', '| --- | --- |',
    '| bar | baz |', '<div>', 'bar']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>', '<th>abc</th>',
    '<th>def</th>', '</tr>', '</thead>', '<tbody>', '<tr>', '<td>bar</td>',
    '<td>baz</td>', '</tr>', '</tbody>', '</table>', '<div>', 'bar']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf201_3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| abc | def | ', '| --- | --- |',
    '| bar | baz |', '<del>', 'bar']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>', '<th>abc</th>',
    '<th>def</th>', '</tr>', '</thead>', '<tbody>', '<tr>', '<td>bar</td>',
    '<td>baz</td>', '</tr>', '<tr>', '<td><del></td>', '<td></td>', '</tr>',
    '<tr>', '<td>bar</td>', '<td></td>', '</tr>', '</tbody>', '</table>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf202;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| abc | def |', '| --- | --- |',
    '| bar | baz |', 'bar', '', 'bar']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>', '<th>abc</th>',
    '<th>def</th>', '</tr>', '</thead>', '<tbody>', '<tr>', '<td>bar</td>',
    '<td>baz</td>', '</tr>', '<tr>', '<td>bar</td>', '<td></td>', '</tr>',
    '</tbody>', '</table>', '<p>bar</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf203;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| abc | def |', '| --- |', '| bar |']);
  Expected := String.Join(WikiLB, ['<p>| abc | def |', '| --- |', '| bar |</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf204;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| abc | def |', '| --- | --- |', '| bar |',
    '| bar | baz | boo |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>', '<th>abc</th>',
    '<th>def</th>', '</tr>', '</thead>', '<tbody>', '<tr>', '<td>bar</td>',
    '<td></td>', '</tr>', '<tr>', '<td>bar</td>', '<td>baz</td>', '</tr>',
    '</tbody>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf205;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['| abc | def |', '| --- | --- |']);
  Expected := String.Join(WikiLB, ['<table>', '<thead>', '<tr>', '<th>abc</th>',
    '<th>def</th>', '</tr>', '</thead>', '</table>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf206;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '> # Foo' + WikiLB + '> bar' + WikiLB + '> baz';
  Expected := '<blockquote>' + WikiLB + '<h1>Foo</h1>' + WikiLB + '<p>bar' +
    WikiLB + 'baz</p>' + WikiLB + '</blockquote>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf207;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '># Foo' + WikiLB + '>bar' + WikiLB + '> baz';
  Expected := '<blockquote>' + WikiLB + '<h1>Foo</h1>' + WikiLB + '<p>bar' +
    WikiLB + 'baz</p>' + WikiLB + '</blockquote>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf208;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '   > # Foo' + WikiLB + '   > bar' + WikiLB + ' > baz';
  Expected := '<blockquote>' + WikiLB + '<h1>Foo</h1>' + WikiLB + '<p>bar' +
    WikiLB + 'baz</p>' + WikiLB + '</blockquote>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf209;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '    > # Foo' + WikiLB + '    > bar' + WikiLB + '    > baz';
  Expected := '<pre><code>&gt; # Foo' + WikiLB + '&gt; bar' + WikiLB +
    '&gt; baz' + WikiLB + '</code></pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf21;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ' - - -';
  Expected := '<hr />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf210;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '> # Foo' + WikiLB + '> bar' + WikiLB + 'baz';
  Expected := '<blockquote>' + WikiLB + '<h1>Foo</h1>' + WikiLB + '<p>bar' +
    WikiLB + 'baz</p>' + WikiLB + '</blockquote>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf211;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '> bar' + WikiLB + 'baz' + WikiLB + '> foo';
  Expected := '<blockquote>' + WikiLB + '<p>bar' + WikiLB + 'baz' + WikiLB +
    'foo</p>' + WikiLB + '</blockquote>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf212;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '> foo' + WikiLB + '---';
  Expected := '<blockquote>' + WikiLB + '<p>foo</p>' + WikiLB + '</blockquote>'
    + WikiLB + '<hr />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf213;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> - foo', '- bar']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<ul>', '<li>foo</li>',
    '</ul>', '</blockquote>', '<ul>', '<li>bar</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf214;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['>     foo', '    bar']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<pre><code>foo',
    '</code></pre>', '</blockquote>', '<pre><code>bar', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf215;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> ```', 'foo', '```']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<pre><code></code></pre>',
    '</blockquote>', '<p>foo</p>', '<pre><code></code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf216;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> foo', '    - bar']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>foo', '- bar</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf217;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '>';
  Expected := String.Join(WikiLB, ['<blockquote>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf218;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '>' + WikiLB + '>  ' + WikiLB + '> ';
  Expected := String.Join(WikiLB, ['<blockquote>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf219;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '>' + WikiLB + '> foo' + WikiLB + '>  ';
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>foo</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf22;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ' **  * ** * ** * **';
  Expected := '<hr />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf220;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> foo', '', '> bar']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>foo</p>',
    '</blockquote>', '<blockquote>', '<p>bar</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf221;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> foo', '> bar']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>foo', 'bar</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf222;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '> foo' + WikiLB + '>' + WikiLB + '> bar';
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>foo</p>', '<p>bar</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf223;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['foo', '> bar']);
  Expected := String.Join(WikiLB, ['<p>foo</p>', '<blockquote>', '<p>bar</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf224;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> aaa', '***', '> bbb']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>aaa</p>',
    '</blockquote>', '<hr />', '<blockquote>', '<p>bbb</p>', '</blockquote>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf225;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> bar', 'baz']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>bar', 'baz</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf226;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> bar', '', 'baz']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>bar</p>',
    '</blockquote>', '<p>baz</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf227;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '> bar' + WikiLB + '>' + WikiLB + 'baz';
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>bar</p>',
    '</blockquote>', '<p>baz</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf228;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> > > foo', 'bar']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<blockquote>',
    '<blockquote>', '<p>foo', 'bar</p>', '</blockquote>', '</blockquote>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf229;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['>>> foo', '> bar', '>>baz']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<blockquote>',
    '<blockquote>', '<p>foo', 'bar', 'baz</p>', '</blockquote>',
    '</blockquote>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf23;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '-     -      -      -';
  Expected := '<hr />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf230;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['>     code', '', '>    not code']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<pre><code>code',
    '</code></pre>', '</blockquote>', '<blockquote>', '<p>not code</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf231;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['A paragraph', 'with two lines.', '',
    '    indented code', '', '> A block quote.']);
  Expected := String.Join(WikiLB, ['<p>A paragraph', 'with two lines.</p>',
    '<pre><code>indented code', '</code></pre>', '<blockquote>',
    '<p>A block quote.</p>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf232;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['1.  A paragraph', '    with two lines.', '',
    '        indented code', '', '    > A block quote.']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<p>A paragraph',
    'with two lines.</p>', '<pre><code>indented code', '</code></pre>',
    '<blockquote>', '<p>A block quote.</p>', '</blockquote>', '</li>', '</ol>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf232_2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' -  A paragraph', '    with two lines.', '',
    '        indented code', '', '    > A block quote.']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>A paragraph',
    'with two lines.</p>', '<pre><code>indented code', '</code></pre>',
    '<blockquote>', '<p>A block quote.</p>', '</blockquote>', '</li>', '</ul>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf233;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- one', '', ' two']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>one</li>', '</ul>',
    '<p>two</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf234;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- one', '', '  two']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>one</p>', '<p>two</p>',
    '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf235;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' -    one', '', '     two']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>one</li>', '</ul>',
    '<pre><code> two', '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf236;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' -    one', '', '      two']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>one</p>', '<p>two</p>',
    '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf237;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['   > > 1.  one', '>>', '>>     two']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<blockquote>', '<ol>',
    '<li>', '<p>one</p>', '<p>two</p>', '</li>', '</ol>', '</blockquote>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf238;
begin
  raise ENotImplemented.Create('Need impliment.');
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf239;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-one', '', '2.two']);
  Expected := String.Join(WikiLB, ['<p>-one</p>', '<p>2.two</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf24;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '- - - -    ';
  Expected := '<hr />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf240;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- foo', '', '', '  bar']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>foo</p>', '<p>bar</p>',
    '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf241;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['1.  foo', '', '    ```', '    bar',
    '    ```', '', '    baz', '', '    > bam']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<p>foo</p>',
    '<pre><code>bar', '</code></pre>', '<p>baz</p>', '<blockquote>',
    '<p>bam</p>', '</blockquote>', '</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf242;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- Foo', '', '      bar', '', '',
    '      baz']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>Foo</p>',
    '<pre><code>bar', '', '', 'baz', '</code></pre>', '</li>', '</ul>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf243;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['123456789. ok']);
  Expected := String.Join(WikiLB, ['<ol start="123456789">', '<li>ok</li>',
    '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf244;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['1234567890. not ok']);
  Expected := String.Join(WikiLB, ['<p>1234567890. not ok</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf245;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['0. ok']);
  Expected := String.Join(WikiLB, ['<ol start="0">', '<li>ok</li>', '</ol>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf246;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['003. ok']);
  Expected := String.Join(WikiLB, ['<ol start="3">', '<li>ok</li>', '</ol>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf247;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-1. not ok']);
  Expected := String.Join(WikiLB, ['<p>-1. not ok</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf248;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- foo', '', '      bar']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>foo</p>',
    '<pre><code>bar', '</code></pre>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf249;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['  10.  foo', '', '           bar']);
  Expected := String.Join(WikiLB, ['<ol start="10">', '<li>', '<p>foo</p>',
    '<pre><code>bar', '</code></pre>', '</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf25;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_ _ _ _ a' + WikiLB + WikiLB + 'a------' + WikiLB + WikiLB +
    '---a---';
  Expected := '<p>_ _ _ _ a</p>' + WikiLB + '<p>a------</p>' + WikiLB +
    '<p>---a---</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf250;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['    indented code', '', 'paragraph', '',
    '    more code']);
  Expected := String.Join(WikiLB, ['<pre><code>indented code', '</code></pre>',
    '<p>paragraph</p>', '<pre><code>more code', '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf251;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['1.     indented code', '', '   paragraph',
    '', '       more code']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<pre><code>indented code',
    '</code></pre>', '<p>paragraph</p>', '<pre><code>more code',
    '</code></pre>', '</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf252;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['1.      indented code', '', '   paragraph',
    '', '       more code']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<pre><code> indented code',
    '</code></pre>', '<p>paragraph</p>', '<pre><code>more code',
    '</code></pre>', '</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf253;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['   foo', '', 'bar']);
  Expected := String.Join(WikiLB, ['<p>foo</p>', '<p>bar</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf254;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-    foo', '', '  bar']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo</li>', '</ul>',
    '<p>bar</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf255;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-  foo', '', '   bar']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>foo</p>', '<p>bar</p>',
    '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf256;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '-' + WikiLB + '  foo' + WikiLB + '-' + WikiLB + '  ```' + WikiLB
    + '  bar' + WikiLB + '  ```' + WikiLB + '-' + WikiLB + '      baz';
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo</li>', '<li>',
    '<pre><code>bar', '</code></pre>', '</li>', '<li>', '<pre><code>baz',
    '</code></pre>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf256_2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '2.' + WikiLB + '   foo' + WikiLB + '3.' + WikiLB + '   ```' +
    WikiLB + '   bar' + WikiLB + '   ```' + WikiLB + '4.' + WikiLB +
    '       baz';
  Expected := String.Join(WikiLB, ['<ol start="2">', '<li>foo</li>', '<li>',
    '<pre><code>bar', '</code></pre>', '</li>', '<li>', '<pre><code>baz',
    '</code></pre>', '</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf257;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-   ', '  foo']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf258;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '-' + WikiLB + '' + WikiLB + '  foo';
  Expected := String.Join(WikiLB, ['<ul>', '<li></li>', '</ul>', '<p>foo</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf259;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '- foo' + WikiLB + '-' + WikiLB + '- bar';
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo</li>', '<li></li>',
    '<li>bar</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf26;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ' *-*';
  Expected := '<p><em>-</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf260;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- foo', '-   ', '- bar']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo</li>', '<li></li>',
    '<li>bar</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf261;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['1. foo', '2.', '3. bar']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>foo</li>', '<li></li>',
    '<li>bar</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf262;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*';
  Expected := String.Join(WikiLB, ['<ul>', '<li></li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf262_2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '123.';
  Expected := String.Join(WikiLB, ['<ol start="123">', '<li></li>', '</ol>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf263;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo' + WikiLB + '*' + WikiLB + '' + WikiLB + 'foo' +
    WikiLB + '1.';
  Expected := String.Join(WikiLB, ['<p>foo', '*</p>', '<p>foo', '1.</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf264;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' 1.  A paragraph', '     with two lines.',
    '', '         indented code', '', '     > A block quote.']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<p>A paragraph',
    'with two lines.</p>', '<pre><code>indented code', '</code></pre>',
    '<blockquote>', '<p>A block quote.</p>', '</blockquote>', '</li>', '</ol>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf265;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['  1.  A paragraph', '      with two lines.',
    '', '          indented code', '', '      > A block quote.']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<p>A paragraph',
    'with two lines.</p>', '<pre><code>indented code', '</code></pre>',
    '<blockquote>', '<p>A block quote.</p>', '</blockquote>', '</li>', '</ol>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf266;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['   1.  A paragraph',
    '       with two lines.', '', '           indented code', '',
    '       > A block quote.']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<p>A paragraph',
    'with two lines.</p>', '<pre><code>indented code', '</code></pre>',
    '<blockquote>', '<p>A block quote.</p>', '</blockquote>', '</li>', '</ol>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf267;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['    1.  A paragraph',
    '        with two lines.', '', '            indented code', '',
    '        > A block quote.']);
  Expected := String.Join(WikiLB, ['<pre><code>1.  A paragraph',
    '    with two lines.', '', '        indented code', '',
    '    &gt; A block quote.', '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf268;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['  1.  A paragraph', 'with two lines.', '',
    '          indented code', '', '      > A block quote.']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<p>A paragraph',
    'with two lines.</p>', '<pre><code>indented code', '</code></pre>',
    '<blockquote>', '<p>A block quote.</p>', '</blockquote>', '</li>', '</ol>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf269;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['  1.  A paragraph', '    with two lines.']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>A paragraph',
    'with two lines.</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf27;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- foo', '***', '- bar']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo</li>', '</ul>', '<hr />',
    '<ul>', '<li>bar</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf270;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> 1. > Blockquote', 'continued here.']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<ol>', '<li>',
    '<blockquote>', '<p>Blockquote', 'continued here.</p>', '</blockquote>',
    '</li>', '</ol>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf271;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> 1. > Blockquote', '> continued here.']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<ol>', '<li>',
    '<blockquote>', '<p>Blockquote', 'continued here.</p>', '</blockquote>',
    '</li>', '</ol>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf272;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- foo', '  - bar', '    - baz',
    '      - boo']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo', '<ul>', '<li>bar', '<ul>',
    '<li>baz', '<ul>', '<li>boo</li>', '</ul>', '</li>', '</ul>', '</li>',
    '</ul>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf273;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- foo', ' - bar', '  - baz', '   - boo']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo</li>', '<li>bar</li>',
    '<li>baz</li>', '<li>boo</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf274;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['10) foo', '    - bar']);
  Expected := String.Join(WikiLB, ['<ol start="10">', '<li>foo', '<ul>',
    '<li>bar</li>', '</ul>', '</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf275;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['10) foo', '   - bar']);
  Expected := String.Join(WikiLB, ['<ol start="10">', '<li>foo</li>', '</ol>',
    '<ul>', '<li>bar</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf276;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '- - foo';
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<ul>', '<li>foo</li>',
    '</ul>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf277;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '1. - 2. foo';
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<ul>', '<li>',
    '<ol start="2">', '<li>foo</li>', '</ol>', '</li>', '</ul>', '</li>',
    '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf278;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- # Foo', '- Bar', '  ---', '  baz']);
  // NOTE: The position of line breaks is arranged.
  Expected := String.Join(WikiLB, ['<ul>', '<li><h1>Foo</h1>', '</li>',
    '<li><h2>Bar</h2>', 'baz</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf279;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- [ ] foo', '- [x] bar']);
  // NOTE: omitted disabled attribute and added task-list-item class
  Expected := String.Join(WikiLB,
    ['<ul>', '<li class="task-list-item"><input type="checkbox"> foo</li>',
    '<li class="task-list-item"><input type="checkbox" checked=""> bar</li>',
    '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf28;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', '***', 'bar']);
  Expected := String.Join(WikiLB, ['<p>Foo</p>', '<hr />', '<p>bar</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf280;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- [x] foo', '  - [ ] bar', '  - [x] baz',
    '- [ ] bim']);
  // NOTE: omitted disabled attribute and added task-list-item class
  Expected := String.Join(WikiLB,
    ['<ul>', '<li class="task-list-item"><input type="checkbox" checked=""> foo',
    '<ul>', '<li class="task-list-item"><input type="checkbox"> bar</li>',
    '<li class="task-list-item"><input type="checkbox" checked=""> baz</li>',
    '</ul>', '</li>',
    '<li class="task-list-item"><input type="checkbox"> bim</li>', '</ul>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf281;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '- foo' + WikiLB + '- bar' + WikiLB + '+ baz';
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo</li>', '<li>bar</li>',
    '</ul>', '<ul>', '<li>baz</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf282;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '1. foo' + WikiLB + '2. bar' + WikiLB + '3) baz';
  Expected := String.Join(WikiLB, ['<ol>', '<li>foo</li>', '<li>bar</li>',
    '</ol>', '<ol start="3">', '<li>baz</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf283;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', '- bar', '- baz']);
  Expected := String.Join(WikiLB, ['<p>Foo</p>', '<ul>', '<li>bar</li>',
    '<li>baz</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf284;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['The number of windows in my house is',
    '14.  The number of doors is 6.']);
  Expected := String.Join(WikiLB, ['<p>The number of windows in my house is',
    '14.  The number of doors is 6.</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf285;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['The number of windows in my house is',
    '1.  The number of doors is 6.']);
  Expected := String.Join(WikiLB,
    ['<p>The number of windows in my house is</p>', '<ol>',
    '<li>The number of doors is 6.</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf286;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- foo', '', '- bar', '', '', '- baz']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>foo</p>', '</li>',
    '<li>', '<p>bar</p>', '</li>', '<li>', '<p>baz</p>', '</li>', '</ul>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf286_2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['1. foo', '', '2. bar', '', '', '3. baz']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<p>foo</p>', '</li>',
    '<li>', '<p>bar</p>', '</li>', '<li>', '<p>baz</p>', '</li>', '</ol>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf287;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- foo', '  - bar', '    - baz', '', '',
    '      bim']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo', '<ul>', '<li>bar', '<ul>',
    '<li>', '<p>baz</p>', '<p>bim</p>', '</li>', '</ul>', '</li>', '</ul>',
    '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf287_2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['1. foo', '   1. bar', '      1. baz', '',
    '', '         bim']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>foo', '<ol>', '<li>bar', '<ol>',
    '<li>', '<p>baz</p>', '<p>bim</p>', '</li>', '</ol>', '</li>', '</ol>',
    '</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf288;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- foo', '- bar', '', '<!-- -->', '',
    '- baz', '- bim']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo</li>', '<li>bar</li>',
    '</ul>', '<!-- -->', '<ul>', '<li>baz</li>', '<li>bim</li>', '</ul>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf289;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['-   foo', '', '    notcode', '', '-   foo',
    '', '<!-- -->', '', '    code']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>foo</p>',
    '<p>notcode</p>', '</li>', '<li>', '<p>foo</p>', '</li>', '</ul>',
    '<!-- -->', '<pre><code>code', '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf29;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', '---', 'bar']);
  Expected := String.Join(WikiLB, ['<h2>Foo</h2>', '<p>bar</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf290;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- a', ' - b', '  - c', '   - d', '  - e',
    ' - f', '- g']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>a</li>', '<li>b</li>',
    '<li>c</li>', '<li>d</li>', '<li>e</li>', '<li>f</li>', '<li>g</li>',
    '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf291;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['1. a', '', '  2. b', '', '   3. c']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<p>a</p>', '</li>', '<li>',
    '<p>b</p>', '</li>', '<li>', '<p>c</p>', '</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf292;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- a', ' - b', '  - c', '   - d',
    '    - e']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>a</li>', '<li>b</li>',
    '<li>c</li>', '<li>d', '- e</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf293;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['1. a', '', '  2. b', '', '    3. c']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<p>a</p>', '</li>', '<li>',
    '<p>b</p>', '</li>', '</ol>', '<pre><code>3. c', '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf294;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- a', '- b', '', '- c']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>a</p>', '</li>', '<li>',
    '<p>b</p>', '</li>', '<li>', '<p>c</p>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf295;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '* a' + WikiLB + '*' + WikiLB + '' + WikiLB + '* c';
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>a</p>', '</li>',
    '<li></li>', '<li>', '<p>c</p>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf296;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- a', '- b', '', '  c', '- d']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>a</p>', '</li>', '<li>',
    '<p>b</p>', '<p>c</p>', '</li>', '<li>', '<p>d</p>', '</li>', '</ul>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf297;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- a', '- b', '', '  [ref]: /url', '- d']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>a</p>', '</li>', '<li>',
    '<p>b</p>', '</li>', '<li>', '<p>d</p>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf298;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- a', '- ```', '  b', '', '',
    '  ```', '- c']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>a</li>', '<li>', '<pre><code>b',
    '', '', '</code></pre>', '</li>', '<li>c</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf299;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- a', '  - b', '', '    c', '- d']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>a', '<ul>', '<li>', '<p>b</p>',
    '<p>c</p>', '</li>', '</ul>', '</li>', '<li>d</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['    a'#9'a', '    ὐ'#9'a']);
  Expected := String.Join(WikiLB, ['<pre><code>a'#9'a', 'ὐ'#9'a',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf30;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['* Foo', '* * *', '* Bar']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>Foo</li>', '</ul>', '<hr />',
    '<ul>', '<li>Bar</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf300;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['* a', '  > b', '  >', '* c']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>a', '<blockquote>', '<p>b</p>',
    '</blockquote>', '</li>', '<li>c</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf301;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- a', '  > b', '  ```', '  c',
    '  ```', '- d']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>a', '<blockquote>', '<p>b</p>',
    '</blockquote>', '<pre><code>c', '</code></pre>', '</li>', '<li>d</li>',
    '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf302;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '- a';
  Expected := String.Join(WikiLB, ['<ul>', '<li>a</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf303;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- a', '  - b']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>a', '<ul>', '<li>b</li>',
    '</ul>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf304;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['1. ```', '   foo', '   ```', '', '   bar']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<pre><code>foo',
    '</code></pre>', '<p>bar</p>', '</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf305;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['* foo', '  * bar', '', '  baz']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>foo</p>', '<ul>',
    '<li>bar</li>', '</ul>', '<p>baz</p>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf306;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- a', '  - b', '  - c', '', '- d', '  - e',
    '  - f']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>a</p>', '<ul>',
    '<li>b</li>', '<li>c</li>', '</ul>', '</li>', '<li>', '<p>d</p>', '<ul>',
    '<li>e</li>', '<li>f</li>', '</ul>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf307;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '`hi`lo`';
  Expected := '<p><code>hi</code>lo`</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf308;
var
  Expected: String;
  Sentence: String;
begin
  Sentence :=
    '\!\"\#\$\%\&\''\(\)\*\+\,\-\.\/\:\;\<\=\>\?\@\[\\\]\^\_\`\{\|\}\~';
  Expected := '<p>!&quot;#$%&amp;''()*+,-./:;&lt;=&gt;?@[\]^_`{|}~</p>'
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf309;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '\' + #9 + '\A\a\ \3\φ\«';
  Expected := '<p>\' + #9 + '\A\a\ \3\φ\«</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf31;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- Foo', '- * * *']);
  // NOTE: The position of line breaks is arranged.
  Expected := String.Join(WikiLB, ['<ul>', '<li>Foo</li>', '<li><hr />',
    '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf310;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['\*not emphasized*', '\<br/> not a tag',
    '\[not a link](/foo)', '\`not code`', '1\. not a list', '\* not a list',
    '\# not a heading', '\[foo]: /url "not a reference"',
    '\&ouml; not a character entity']);
  Expected := String.Join(WikiLB, ['<p>*not emphasized*',
    '&lt;br/&gt; not a tag', '[not a link](/foo)', '`not code`',
    '1. not a list', '* not a list', '# not a heading',
    '[foo]: /url &quot;not a reference&quot;',
    '&amp;ouml; not a character entity</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf311;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '\\*emphasis*';
  Expected := '<p>\<em>emphasis</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf312;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['foo\', 'bar']);
  Expected := String.Join(WikiLB, ['<p>foo<br />', 'bar</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf313;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '`` \[\` ``';
  Expected := '<p><code>\[\`</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf314;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '    \[\]';
  Expected := String.Join(WikiLB, ['<pre><code>\[\]', '</code></pre>'])
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf315;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['~~~', '\[\]', '~~~']);
  Expected := String.Join(WikiLB, ['<pre><code>\[\]', '</code></pre>'])
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf316;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<http://example.com?find=\*>';
  Expected :=
    '<p><a href="http://example.com?find=%5C*">http://example.com?find=\*</a></p>'
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf317;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<a href="/bar\/)">';
  Expected := '<a href="/bar\/)">' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf318;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[foo](/bar\* "ti\*tle")';
  Expected := '<p><a href="/bar*" title="ti*tle">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf319;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]', '', '[foo]: /bar\* "ti\*tle"']);
  Expected := '<p><a href="/bar*" title="ti*tle">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf32;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '# foo' + WikiLB + '## foo' + WikiLB + '### foo' + WikiLB +
    '#### foo' + WikiLB + '##### foo' + WikiLB + '###### foo';
  Expected := '<h1>foo</h1>' + WikiLB + '<h2>foo</h2>' + WikiLB + '<h3>foo</h3>'
    + WikiLB + '<h4>foo</h4>' + WikiLB + '<h5>foo</h5>' + WikiLB +
    '<h6>foo</h6>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf320;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['``` foo\+bar', 'foo', '```']);
  Expected := String.Join(WikiLB, ['<pre><code class="language-foo+bar">foo',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf321;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['&nbsp; &amp; &copy; &AElig; &Dcaron;',
    '&frac34; &HilbertSpace; &DifferentialD;',
    '&ClockwiseContourIntegral; &ngE;']);
  Expected := String.Join(WikiLB, ['<p>'#$00a0' &amp; © Æ Ď', '¾ ℋ ⅆ',
    '∲ ≧̸</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf322;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '&#35; &#1234; &#992; &#0;';
  Expected := '<p># Ӓ Ϡ �</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf323;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '&#X22; &#XD06; &#xcab;';
  Expected := '<p>&quot; ആ ಫ</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf324;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['&nbsp &x; &#; &#x;', '&#87654321;',
    '&#abcdef0;', '&ThisIsNotDefined; &hi?;']);
  Expected := String.Join(WikiLB, ['<p>&amp;nbsp &amp;x; &amp;#; &amp;#x;',
    '&amp;#87654321;', '&amp;#abcdef0;', '&amp;ThisIsNotDefined; &amp;hi?;</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf325;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '&copy';
  Expected := '<p>&amp;copy</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf326;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '&MadeUpEntity;';
  Expected := '<p>&amp;MadeUpEntity;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf327;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<a href="&ouml;&ouml;.html">';
  Expected := '<a href="&ouml;&ouml;.html">' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf328;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[foo](/f&ouml;&ouml; "f&ouml;&ouml;")';
  Expected := '<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf329;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB,
    ['[foo]', '', '[foo]: /f&ouml;&ouml; "f&ouml;&ouml;"']);
  Expected := '<p><a href="/f%C3%B6%C3%B6" title="föö">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf33;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '####### foo';
  Expected := '<p>####### foo</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf330;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['``` f&ouml;&ouml;', 'foo', '```']);
  Expected := String.Join(WikiLB, ['<pre><code class="language-föö">foo',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf331;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '`f&ouml;&ouml;`';
  Expected := '<p><code>f&amp;ouml;&amp;ouml;</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf332;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '    f&ouml;f&ouml;';
  Expected := String.Join(WikiLB, ['<pre><code>f&amp;ouml;f&amp;ouml;',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf333;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['&#42;foo&#42;', '*foo*']);
  Expected := String.Join(WikiLB, ['<p>*foo*', '<em>foo</em></p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf334;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['&#42; foo', '', '* foo']);
  Expected := String.Join(WikiLB, ['<p>* foo</p>', '<ul>', '<li>foo</li>',
    '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf335;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo&#10;&#10;bar';
  Expected := '<p>foo'#10#10'bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf336;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '&#9;foo';
  Expected := '<p>'#9'foo</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf337;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[a](url &quot;tit&quot;)';
  Expected := '<p>[a](url &quot;tit&quot;)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf338;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '`foo`';
  Expected := '<p><code>foo</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf339;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '`` foo ` bar ``';
  Expected := '<p><code>foo ` bar</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf34;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '#5 bolt' + WikiLB + WikiLB + '#hashtag';
  Expected := '<p>#5 bolt</p>' + WikiLB + '<p>#hashtag</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf340;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '` `` `';
  Expected := '<p><code>``</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf341;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '`  ``  `';
  Expected := '<p><code> `` </code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf342;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '` a`';
  Expected := '<p><code> a</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf343;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '`'#$00a0'b'#$00a0'`';
  Expected := '<p><code>'#$00a0'b'#$00a0'</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf344;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['`'#$00a0'`', '`  `']);
  Expected := String.Join(WikiLB, ['<p><code>'#$00a0'</code>',
    '<code>  </code></p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf345;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['``', 'foo', 'bar  ', 'baz', '``']);
  Expected := '<p><code>foo bar   baz</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf346;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['``', 'foo ', '``']);
  Expected := '<p><code>foo </code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf347;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['`foo   bar ', 'baz`']);
  Expected := '<p><code>foo   bar  baz</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf348;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '`foo\`bar`';
  Expected := '<p><code>foo\</code>bar`</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf349;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '``foo`bar``';
  Expected := '<p><code>foo`bar</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf35;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '\## foo';
  Expected := '<p>## foo</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf350;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '` foo `` bar `';
  Expected := '<p><code>foo `` bar</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf351;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo`*`';
  Expected := '<p>*foo<code>*</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf352;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[not a `link](/foo`)';
  Expected := '<p>[not a <code>link](/foo</code>)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf353;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '`<a href="`">`';
  Expected := '<p><code>&lt;a href=&quot;</code>&quot;&gt;`</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf354;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<a href="`">`';
  Expected := '<p><a href="`">`</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf355;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '`<http://foo.bar.`baz>`';
  Expected := '<p><code>&lt;http://foo.bar.</code>baz&gt;`</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf356;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<http://foo.bar.`baz>`';
  Expected :=
    '<p><a href="http://foo.bar.%60baz">http://foo.bar.`baz</a>`</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf357;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '```foo``';
  Expected := '<p>```foo``</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf358;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '`foo';
  Expected := '<p>`foo</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf359;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '`foo``bar``';
  Expected := '<p>`foo<code>bar</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf36;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '# foo *bar* \*baz\*';
  Expected := '<h1>foo <em>bar</em> *baz*</h1>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf360;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo bar*';
  Expected := '<p><em>foo bar</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf361;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'a * foo bar*';
  Expected := '<p>a * foo bar*</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf362;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'a*"foo"*';
  Expected := '<p>a*&quot;foo&quot;*</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf363;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*'#$00a0'a'#$00a0'*';
  Expected := '<p>*'#$00a0'a'#$00a0'*</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf364;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo*bar*';
  Expected := '<p>foo<em>bar</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf365;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '5*6*78';
  Expected := '<p>5<em>6</em>78</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf366;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_foo bar_';
  Expected := '<p><em>foo bar</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf367;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_ foo bar_';
  Expected := '<p>_ foo bar_</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf368;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'a_"foo"_';
  Expected := '<p>a_&quot;foo&quot;_</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf369;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo_bar_';
  Expected := '<p>foo_bar_</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf37;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '#                  foo                     ';
  Expected := '<h1>foo</h1>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf370;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '5_6_78';
  Expected := '<p>5_6_78</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf371;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'пристаням_стремятся_';
  Expected := '<p>пристаням_стремятся_</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf372;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'aa_"bb"_cc';
  Expected := '<p>aa_&quot;bb&quot;_cc</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf373;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo-_(bar)_';
  Expected := '<p>foo-<em>(bar)</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf374;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_foo*';
  Expected := '<p>_foo*</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf375;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo bar *';
  Expected := '<p>*foo bar *</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf376;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo bar' + WikiLB + '*';
  Expected := String.Join(WikiLB, ['<p>*foo bar', '*</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf377;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*(*foo)';
  Expected := '<p>*(*foo)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf378;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*(*foo*)*';
  Expected := '<p><em>(<em>foo</em>)</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf379;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo*bar';
  Expected := '<p><em>foo</em>bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf38;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := ' ### foo' + WikiLB + '  ## foo' + WikiLB + '   # foo';
  Expected := '<h3>foo</h3>' + WikiLB + '<h2>foo</h2>' + WikiLB +
    '<h1>foo</h1>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf380;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_foo bar _';
  Expected := '<p>_foo bar _</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf381;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_(_foo)';
  Expected := '<p>_(_foo)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf382;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_(_foo_)_';
  Expected := '<p><em>(<em>foo</em>)</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf383;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_foo_bar';
  Expected := '<p>_foo_bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf384;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_пристаням_стремятся';
  Expected := '<p>_пристаням_стремятся</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf385;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_foo_bar_baz_';
  Expected := '<p><em>foo_bar_baz</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf386;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_(bar)_.';
  Expected := '<p><em>(bar)</em>.</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf387;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo bar**';
  Expected := '<p><strong>foo bar</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf388;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '** foo bar**';
  Expected := '<p>** foo bar**</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf389;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'a**"foo"**';
  Expected := '<p>a**&quot;foo&quot;**</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf39;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '    # foo';
  Expected := '<pre><code># foo' + WikiLB + '</code></pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf390;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo**bar**';
  Expected := '<p>foo<strong>bar</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf391;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__foo bar__';
  Expected := '<p><strong>foo bar</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf392;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__ foo bar__';
  Expected := '<p>__ foo bar__</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf393;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['__', 'foo bar__']);
  Expected := String.Join(WikiLB, ['<p>__', 'foo bar__</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf394;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'a__"foo"__';
  Expected := '<p>a__&quot;foo&quot;__</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf395;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo__bar__';
  Expected := '<p>foo__bar__</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf396;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '5__6__78';
  Expected := '<p>5__6__78</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf397;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'пристаням__стремятся__';
  Expected := '<p>пристаням__стремятся__</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf398;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__foo, __bar__, baz__';
  Expected := '<p><strong>foo, <strong>bar</strong>, baz</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf399;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo-__(bar)__';
  Expected := '<p>foo-<strong>(bar)</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf4;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['  - foo', '', #9'bar']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>foo</p>', '<p>bar</p>',
    '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf40;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo' + WikiLB + '    # bar';
  Expected := '<p>foo' + WikiLB + '# bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf400;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo bar **';
  Expected := '<p>**foo bar **</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf401;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**(**foo)';
  Expected := '<p>**(**foo)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf402;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*(**foo**)*';
  Expected := '<p><em>(<strong>foo</strong>)</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf403;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB,
    ['**Gomphocarpus (*Gomphocarpus physocarpus*, syn.',
    '*Asclepias physocarpa*)**']);
  Expected := String.Join(WikiLB,
    ['<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>, syn.',
    '<em>Asclepias physocarpa</em>)</strong></p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf404;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo "*bar*" foo**';
  Expected :=
    '<p><strong>foo &quot;<em>bar</em>&quot; foo</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf405;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo**bar';
  Expected := '<p><strong>foo</strong>bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf406;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__foo bar __';
  Expected := '<p>__foo bar __</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf407;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__(__foo)';
  Expected := '<p>__(__foo)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf408;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_(__foo__)_';
  Expected := '<p><em>(<strong>foo</strong>)</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf409;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__foo__bar';
  Expected := '<p>__foo__bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf41;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '## foo ##' + WikiLB + '  ###   bar    ###';
  Expected := '<h2>foo</h2>' + WikiLB + '<h3>bar</h3>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf410;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__пристаням__стремятся';
  Expected := '<p>__пристаням__стремятся</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf411;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__foo__bar__baz__';
  Expected := '<p><strong>foo__bar__baz</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf412;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__(bar)__.';
  Expected := '<p><strong>(bar)</strong>.</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf413;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo [bar](/url)*';
  Expected := '<p><em>foo <a href="/url">bar</a></em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf414;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['*foo', 'bar*']);
  Expected := String.Join(WikiLB, ['<p><em>foo', 'bar</em></p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf415;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_foo __bar__ baz_';
  Expected := '<p><em>foo <strong>bar</strong> baz</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf416;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_foo _bar_ baz_';
  Expected := '<p><em>foo <em>bar</em> baz</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf417;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__foo_ bar_';
  Expected := '<p><em><em>foo</em> bar</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf418;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo *bar**';
  Expected := '<p><em>foo <em>bar</em></em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf419;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo **bar** baz*';
  Expected := '<p><em>foo <strong>bar</strong> baz</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf42;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '# foo ##################################' + WikiLB +
    '##### foo ##';
  Expected := '<h1>foo</h1>' + WikiLB + '<h5>foo</h5>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf420;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo**bar**baz*';
  Expected := '<p><em>foo<strong>bar</strong>baz</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf421;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo**bar*';
  Expected := '<p><em>foo**bar</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf422;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '***foo** bar*';
  Expected := '<p><em><strong>foo</strong> bar</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf423;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo **bar***';
  Expected := '<p><em>foo <strong>bar</strong></em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf424;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo**bar***';
  Expected := '<p><em>foo<strong>bar</strong></em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf425;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo***bar***baz';
  Expected := '<p>foo<em><strong>bar</strong></em>baz</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf426;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo******bar*********baz';
  Expected :=
    '<p>foo<strong><strong><strong>bar</strong></strong></strong>***baz</p>' +
    WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf427;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo **bar *baz* bim** bop*';
  Expected :=
    '<p><em>foo <strong>bar <em>baz</em> bim</strong> bop</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf428;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo [*bar*](/url)*';
  Expected := '<p><em>foo <a href="/url"><em>bar</em></a></em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf429;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '** is not an empty emphasis';
  Expected := '<p>** is not an empty emphasis</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf43;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '### foo ###     ';
  Expected := '<h3>foo</h3>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf430;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**** is not an empty strong emphasis';
  Expected := '<p>**** is not an empty strong emphasis</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf431;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo [bar](/url)**';
  Expected := '<p><strong>foo <a href="/url">bar</a></strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf432;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['**foo', 'bar**']);
  Expected := String.Join(WikiLB, ['<p><strong>foo', 'bar</strong></p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf433;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__foo _bar_ baz__';
  Expected := '<p><strong>foo <em>bar</em> baz</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf434;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__foo __bar__ baz__';
  Expected := '<p><strong>foo <strong>bar</strong> baz</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf435;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '____foo__ bar__';
  Expected := '<p><strong><strong>foo</strong> bar</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf436;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo **bar****';
  Expected := '<p><strong>foo <strong>bar</strong></strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf437;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo *bar* baz**';
  Expected := '<p><strong>foo <em>bar</em> baz</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf438;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo*bar*baz**';
  Expected := '<p><strong>foo<em>bar</em>baz</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf439;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '***foo* bar**';
  Expected := '<p><strong><em>foo</em> bar</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf44;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '### foo ### b';
  Expected := '<h3>foo ### b</h3>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf440;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo *bar***';
  Expected := '<p><strong>foo <em>bar</em></strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf441;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['**foo *bar **baz**', 'bim* bop**']);
  Expected := String.Join(WikiLB,
    ['<p><strong>foo <em>bar <strong>baz</strong>', 'bim</em> bop</strong></p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf442;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo [*bar*](/url)**';
  Expected :=
    '<p><strong>foo <a href="/url"><em>bar</em></a></strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf443;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__ is not an empty emphasis';
  Expected := '<p>__ is not an empty emphasis</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf444;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '____ is not an empty strong emphasis';
  Expected := '<p>____ is not an empty strong emphasis</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf445;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo ***';
  Expected := '<p>foo ***</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf446;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo *\**';
  Expected := '<p>foo <em>*</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf447;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo *_*';
  Expected := '<p>foo <em>_</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf448;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo *****';
  Expected := '<p>foo *****</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf449;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo **\***';
  Expected := '<p>foo <strong>*</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf45;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '# foo#';
  Expected := '<h1>foo#</h1>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf450;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo **_**';
  Expected := '<p>foo <strong>_</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf451;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo*';
  Expected := '<p>*<em>foo</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf452;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo**';
  Expected := '<p><em>foo</em>*</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf453;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '***foo**';
  Expected := '<p>*<strong>foo</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf454;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '****foo*';
  Expected := '<p>***<em>foo</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf455;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo***';
  Expected := '<p><strong>foo</strong>*</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf456;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo****';
  Expected := '<p><em>foo</em>***</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf457;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo ___';
  Expected := '<p>foo ___</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf458;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo _\__';
  Expected := '<p>foo <em>_</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf459;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo _*_';
  Expected := '<p>foo <em>*</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf46;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '### foo \###' + WikiLB + '## foo #\##' + WikiLB + '# foo \#';
  Expected := '<h3>foo ###</h3>' + WikiLB + '<h2>foo ###</h2>' + WikiLB +
    '<h1>foo #</h1>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf460;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo _____';
  Expected := '<p>foo _____</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf461;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo __\___';
  Expected := '<p>foo <strong>_</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf462;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo __*__';
  Expected := '<p>foo <strong>*</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf463;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__foo_';
  Expected := '<p>_<em>foo</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf464;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_foo__';
  Expected := '<p><em>foo</em>_</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf465;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '___foo__';
  Expected := '<p>_<strong>foo</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf466;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '____foo_';
  Expected := '<p>___<em>foo</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf467;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__foo___';
  Expected := '<p><strong>foo</strong>_</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf468;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_foo____';
  Expected := '<p><em>foo</em>___</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf469;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo**';
  Expected := '<p><strong>foo</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf47;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '****' + WikiLB + '## foo' + WikiLB + '****';
  Expected := '<hr />' + WikiLB + '<h2>foo</h2>' + WikiLB + '<hr />' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf470;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*_foo_*';
  Expected := '<p><em><em>foo</em></em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf471;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__foo__';
  Expected := '<p><strong>foo</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf472;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_*foo*_';
  Expected := '<p><em><em>foo</em></em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf473;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '****foo****';
  Expected := '<p><strong><strong>foo</strong></strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf474;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '____foo____';
  Expected := '<p><strong><strong>foo</strong></strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf475;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '******foo******';
  Expected :=
    '<p><strong><strong><strong>foo</strong></strong></strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf476;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '***foo***';
  Expected := '<p><em><strong>foo</strong></em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf477;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_____foo_____';
  Expected := '<p><em><strong><strong>foo</strong></strong></em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf478;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo _bar* baz_';
  Expected := '<p><em>foo _bar</em> baz_</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf479;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo __bar *baz bim__ bam*';
  Expected := '<p><em>foo <strong>bar *baz bim</strong> bam</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf48;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'Foo bar' + WikiLB + '# baz' + WikiLB + 'Bar foo';
  Expected := '<p>Foo bar</p>' + WikiLB + '<h1>baz</h1>' + WikiLB +
    '<p>Bar foo</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf480;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**foo **bar baz**';
  Expected := '<p>**foo <strong>bar baz</strong></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf481;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo *bar baz*';
  Expected := '<p>*foo <em>bar baz</em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf482;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*[bar*](/url)';
  Expected := '<p>*<a href="/url">bar*</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf483;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_foo [bar_](/url)';
  Expected := '<p>_foo <a href="/url">bar_</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf484;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*<img src="foo" title="*"/>';
  Expected := '<p>*<img src="foo" title="*"/></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf485;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**<a href="**">';
  Expected := '<p>**<a href="**"></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf486;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__<a href="__">';
  Expected := '<p>__<a href="__"></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf487;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*a `*`*';
  Expected := '<p><em>a <code>*</code></em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf488;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '_a `_`_';
  Expected := '<p><em>a <code>_</code></em></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf489;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '**a<http://foo.bar/?q=**>';
  Expected :=
    '<p>**a<a href="http://foo.bar/?q=**">http://foo.bar/?q=**</a></p>'
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf49;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '## ' + WikiLB + '#' + WikiLB + '### ###';
  Expected := '<h2></h2>' + WikiLB + '<h1></h1>' + WikiLB + '<h3></h3>'
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf490;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '__a<http://foo.bar/?q=__>';
  Expected :=
    '<p>__a<a href="http://foo.bar/?q=__">http://foo.bar/?q=__</a></p>'
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf491;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '~~Hi~~ Hello, ~there~ world!';
  Expected := '<p><del>Hi</del> Hello, <del>there</del> world!</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf492;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['This ~~has a', '', 'new paragraph~~.']);
  Expected := String.Join(WikiLB, ['<p>This ~~has a</p>',
    '<p>new paragraph~~.</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf493;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'This will ~~~not~~~ strike.';
  Expected := '<p>This will ~~~not~~~ strike.</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf494;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](/uri "title")';
  Expected := '<p><a href="/uri" title="title">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf495;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](/uri)';
  Expected := '<p><a href="/uri">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf496;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link]()';
  Expected := '<p><a href="">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf497;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](<>)';
  Expected := '<p><a href="">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf498;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](/my uri)';
  Expected := '<p>[link](/my uri)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf499;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](</my uri>)';
  Expected := '<p><a href="/my%20uri">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf5;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- foo', '', #9#9'bar']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>foo</p>',
    '<pre><code>  bar', '</code></pre>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf50;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo *bar*', '=========', '', 'Foo *bar*',
    '---------']);
  Expected := String.Join(WikiLB, ['<h1>Foo <em>bar</em></h1>',
    '<h2>Foo <em>bar</em></h2>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf500;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[link](foo', 'bar)']);
  Expected := String.Join(WikiLB, ['<p>[link](foo', 'bar)</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf501;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[link](<foo', 'bar>)']);
  Expected := String.Join(WikiLB, ['<p>[link](<foo', 'bar>)</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf502;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[a](<b)c>)';
  Expected := '<p><a href="b)c">a</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf503;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](<foo\>)';
  Expected := '<p>[link](&lt;foo&gt;)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf504;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[a](<b)c', '[a](<b)c>', '[a](<b>c)']);
  Expected := String.Join(WikiLB, ['<p>[a](&lt;b)c', '[a](&lt;b)c&gt;',
    '[a](<b>c)</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf505;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](\(foo\))';
  Expected := '<p><a href="(foo)">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf506;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](foo(and(bar)))';
  Expected := '<p><a href="foo(and(bar))">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf507;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](foo\(and\(bar\))';
  Expected := '<p><a href="foo(and(bar)">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf508;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](<foo(and(bar)>)';
  Expected := '<p><a href="foo(and(bar)">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf509;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](foo\)\:)';
  Expected := '<p><a href="foo):">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf51;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo *bar', 'baz*', '====']);
  Expected := String.Join(WikiLB, ['<h1>Foo <em>bar', 'baz</em></h1>'])
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf510;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[link](#fragment)', '',
    '[link](http://example.com#fragment)', '',
    '[link](http://example.com?foo=3#frag)']);
  Expected := String.Join(WikiLB, ['<p><a href="#fragment">link</a></p>',
    '<p><a href="http://example.com#fragment">link</a></p>',
    '<p><a href="http://example.com?foo=3#frag">link</a></p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf511;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](foo\bar)';
  Expected := '<p><a href="foo%5Cbar">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf512;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](foo%20b&auml;)';
  Expected := '<p><a href="foo%20b%C3%A4">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf513;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link]("title")';
  Expected := '<p><a href="%22title%22">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf514;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[link](/url "title")',
    '[link](/url ''title'')', '[link](/url (title))']);
  Expected := String.Join(WikiLB, ['<p><a href="/url" title="title">link</a>',
    '<a href="/url" title="title">link</a>',
    '<a href="/url" title="title">link</a></p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf515;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](/url "title \"&quot;")';
  Expected :=
    '<p><a href="/url" title="title &quot;&quot;">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf516;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](/url'#$00a0'"title")';
  Expected := '<p><a href="/url%C2%A0%22title%22">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf517;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](/url "title "and" title")';
  Expected :=
    '<p>[link](/url &quot;title &quot;and&quot; title&quot;)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf518;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link](/url ''title "and" title'')';
  Expected :=
    '<p><a href="/url" title="title &quot;and&quot; title">link</a></p>' +
    WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf519;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[link](   /uri', '  "title"  )']);
  Expected := '<p><a href="/uri" title="title">link</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf52;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['  Foo *bar', 'baz*'#9, '====']);
  Expected := String.Join(WikiLB, ['<h1>Foo <em>bar', 'baz</em></h1>'])
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf520;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link] (/uri)';
  Expected := '<p>[link] (/uri)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf521;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link [foo [bar]]](/uri)';
  Expected := '<p><a href="/uri">link [foo [bar]]</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf522;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link] bar](/uri)';
  Expected := '<p>[link] bar](/uri)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf523;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link [bar](/uri)';
  Expected := '<p>[link <a href="/uri">bar</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf524;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link \[bar](/uri)';
  Expected := '<p><a href="/uri">link [bar</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf525;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[link *foo **bar** `#`*](/uri)';
  Expected :=
    '<p><a href="/uri">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>'
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf526;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[![moon](moon.jpg)](/uri)';
  Expected :=
    '<p><a href="/uri"><img src="moon.jpg" alt="moon" /></a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf527;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[foo [bar](/uri)](/uri)';
  Expected := '<p>[foo <a href="/uri">bar</a>](/uri)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf527_2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[foo ![bar](/uri)](/uri)';
  Expected :=
    '<p><a href="/uri">foo <img src="/uri" alt="bar" /></a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf528;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[foo *[bar [baz](/uri)](/uri)*](/uri)';
  Expected :=
    '<p>[foo <em>[bar <a href="/uri">baz</a>](/uri)</em>](/uri)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf529;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '![[[foo](uri1)](uri2)](uri3)';
  Expected := '<p><img src="uri3" alt="[foo](uri2)" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf53;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'Foo' + WikiLB + '-------------------------' + WikiLB + '' +
    WikiLB + 'Foo' + WikiLB + '=';
  Expected := String.Join(WikiLB, ['<h2>Foo</h2>', '<h1>Foo</h1>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf530;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*[foo*](/uri)';
  Expected := '<p>*<a href="/uri">foo*</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf531;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[foo *bar](baz*)';
  Expected := '<p><a href="baz*">foo *bar</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf532;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo [bar* baz]';
  Expected := '<p><em>foo [bar</em> baz]</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf533;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[foo <bar attr="](baz)">';
  Expected := '<p>[foo <bar attr="](baz)"></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf534;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[foo`](/uri)`';
  Expected := '<p>[foo<code>](/uri)</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf535;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '*foo [bar* baz]';
  Expected := '<p><em>foo [bar</em> baz]</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf536;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo][bar]', '', '[bar]: /url "title"']);
  Expected := '<p><a href="/url" title="title">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf537;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[link [foo [bar]]][ref]', '',
    '[ref]: /uri']);
  Expected := '<p><a href="/uri">link [foo [bar]]</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf538;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[link \[bar][ref]', '', '[ref]: /uri']);
  Expected := '<p><a href="/uri">link [bar</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf539;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[link *foo **bar** `#`*][ref]', '',
    '[ref]: /uri']);
  Expected :=
    '<p><a href="/uri">link <em>foo <strong>bar</strong> <code>#</code></em></a></p>'
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf54;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['   Foo', '---', '', '  Foo', '-----', '',
    '  Foo', '  ===']);
  Expected := String.Join(WikiLB, ['<h2>Foo</h2>', '<h2>Foo</h2>',
    '<h1>Foo</h1>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf540;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[![moon](moon.jpg)][ref]', '',
    '[ref]: /uri']);
  Expected :=
    '<p><a href="/uri"><img src="moon.jpg" alt="moon" /></a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf541;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo [bar](/uri)][ref]', '',
    '[ref]: /uri']);
  Expected :=
    '<p>[foo <a href="/uri">bar</a>]<a href="/uri">ref</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf542;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo *bar [baz][ref]*][ref]', '',
    '[ref]: /uri']);
  Expected :=
    '<p>[foo <em>bar <a href="/uri">baz</a></em>]<a href="/uri">ref</a></p>' +
    WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf543;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['*[foo*][ref]', '', '[ref]: /uri']);
  Expected := '<p>*<a href="/uri">foo*</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf544;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo *bar][ref]*', '', '[ref]: /uri']);
  Expected := '<p><a href="/uri">foo *bar</a>*</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf545;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo <bar attr="][ref]">', '',
    '[ref]: /uri']);
  Expected := '<p>[foo <bar attr="][ref]"></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf546;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo`][ref]`', '', '[ref]: /uri']);
  Expected := '<p>[foo<code>][ref]</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf547;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo<http://example.com/?search=][ref]>',
    '', '[ref]: /uri']);
  Expected :=
    '<p>[foo<a href="http://example.com/?search=%5D%5Bref%5D">http://example.com/?search=][ref]</a></p>'
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf548;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo][BaR]', '', '[bar]: /url "title"']);
  Expected := '<p><a href="/url" title="title">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf549;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[ẞ]', '', '[SS]: /url']);
  Expected := '<p><a href="/url">ẞ</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf55;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['    Foo', '    ---', '', '    Foo', '---']);
  Expected := String.Join(WikiLB, ['<pre><code>Foo', '---', '', 'Foo',
    '</code></pre>', '<hr />']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf550;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[Foo', '  bar]: /url', '',
    '[Baz][Foo bar]']);
  Expected := '<p><a href="/url">Baz</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf550_2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[Foo', '  bar]: /url', '', '[Baz][Foo',
    ' bar]']);
  Expected := '<p><a href="/url">Baz</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf551;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo] [bar]', '', '[bar]: /url "title"']);
  Expected := '<p>[foo] <a href="/url" title="title">bar</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf552;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]', '[bar]', '',
    '[bar]: /url "title"']);
  Expected := String.Join(WikiLB,
    ['<p>[foo]', '<a href="/url" title="title">bar</a></p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf553;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]: /url1', '', '[foo]: /url2', '',
    '[bar][foo]']);
  Expected := '<p><a href="/url1">bar</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf554;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[bar][foo\!]', '', '[foo!]: /url']);
  Expected := '<p>[bar][foo!]</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf555;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo][ref[]', '', '[ref[]: /uri']);
  Expected := String.Join(WikiLB, ['<p>[foo][ref[]</p>', '<p>[ref[]: /uri</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf556;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo][ref[bar]]', '', '[ref[bar]]: /uri']);
  Expected := String.Join(WikiLB, ['<p>[foo][ref[bar]]</p>',
    '<p>[ref[bar]]: /uri</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf557;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[[[foo]]]', '', '[[[foo]]]: /url']);
  Expected := String.Join(WikiLB, ['<p>[[[foo]]]</p>', '<p>[[[foo]]]: /url</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf558;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo][ref\[]', '', '[ref\[]: /uri']);
  Expected := '<p><a href="/uri">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf559;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[bar\\]: /uri', '', '[bar\\]']);
  Expected := '<p><a href="/uri">bar\</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf56;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', '   ----      ']);
  Expected := '<h2>Foo</h2>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf560;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[]', '', '[]: /uri']);
  Expected := String.Join(WikiLB, ['<p>[]</p>', '<p>[]: /uri</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf561;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '[' + WikiLB + ' ]' + WikiLB + '' + WikiLB + '[' + WikiLB +
    ' ]: /uri';
  Expected := String.Join(WikiLB, ['<p>[', ']</p>', '<p>[', ']: /uri</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf562;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo][]', '', '[foo]: /url "title"']);
  Expected := '<p><a href="/url" title="title">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf563;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[*foo* bar][]', '',
    '[*foo* bar]: /url "title"']);
  Expected :=
    '<p><a href="/url" title="title"><em>foo</em> bar</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf564;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[Foo][]', '', '[foo]: /url "title"']);
  Expected := '<p><a href="/url" title="title">Foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf565;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo] ', '[]', '', '[foo]: /url "title"']);
  Expected := String.Join(WikiLB, ['<p><a href="/url" title="title">foo</a>',
    '[]</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf566;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]', '', '[foo]: /url "title"']);
  Expected := '<p><a href="/url" title="title">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf567;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[*foo* bar]', '',
    '[*foo* bar]: /url "title"']);
  Expected :=
    '<p><a href="/url" title="title"><em>foo</em> bar</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf568;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[[*foo* bar]]', '',
    '[*foo* bar]: /url "title"']);
  Expected :=
    '<p>[<a href="/url" title="title"><em>foo</em> bar</a>]</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf569;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[[bar [foo]', '', '[foo]: /url']);
  Expected := '<p>[[bar <a href="/url">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf57;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', '    ---']);
  Expected := String.Join(WikiLB, ['<p>Foo', '---</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf570;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[Foo]', '', '[foo]: /url "title"']);
  Expected := '<p><a href="/url" title="title">Foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf571;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo] bar', '', '[foo]: /url']);
  Expected := '<p><a href="/url">foo</a> bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf572;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['\[foo]', '', '[foo]: /url "title"']);
  Expected := '<p>[foo]</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf573;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo*]: /url', '', '*[foo*]']);
  Expected := '<p>*<a href="/url">foo*</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf574;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo][bar]', '', '[foo]: /url1',
    '[bar]: /url2']);
  Expected := '<p><a href="/url2">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf575;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo][]', '', '[foo]: /url1']);
  Expected := '<p><a href="/url1">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf576;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo]()', '', '[foo]: /url1']);
  Expected := '<p><a href="">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf577;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo](not a link)', '', '[foo]: /url1']);
  Expected := '<p><a href="/url1">foo</a>(not a link)</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf578;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo][bar][baz]', '', '[baz]: /url']);
  Expected := '<p>[foo]<a href="/url">bar</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf579;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo][bar][baz]', '', '[baz]: /url1',
    '[bar]: /url2']);
  Expected := '<p><a href="/url2">foo</a><a href="/url1">baz</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf58;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', '= =', '', 'Foo', '--- -']);
  Expected := String.Join(WikiLB, ['<p>Foo', '= =</p>', '<p>Foo</p>', '<hr />']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf580;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['[foo][bar][baz]', '', '[baz]: /url1',
    '[foo]: /url2']);
  Expected := '<p>[foo]<a href="/url1">bar</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf581;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '![foo](/url "title")';
  Expected := '<p><img src="/url" alt="foo" title="title" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf582;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['![foo *bar*]', '',
    '[foo *bar*]: train.jpg "train & tracks"']);
  Expected :=
    '<p><img src="train.jpg" alt="foo bar" title="train &amp; tracks" /></p>' +
    WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf583;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '![foo ![bar](/url)](/url2)';
  Expected := '<p><img src="/url2" alt="foo bar" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf584;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '![foo [bar](/url)](/url2)';
  Expected := '<p><img src="/url2" alt="foo bar" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf585;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['![foo *bar*][]', '',
    '[foo *bar*]: train.jpg "train & tracks"']);
  Expected :=
    '<p><img src="train.jpg" alt="foo bar" title="train &amp; tracks" /></p>' +
    WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf586;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['![foo *bar*][foobar]', '',
    '[FOOBAR]: train.jpg "train & tracks"']);
  Expected :=
    '<p><img src="train.jpg" alt="foo bar" title="train &amp; tracks" /></p>' +
    WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf587;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '![foo](train.jpg)';
  Expected := '<p><img src="train.jpg" alt="foo" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf588;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'My ![foo bar](/path/to/train.jpg  "title"   )';
  Expected :=
    '<p>My <img src="/path/to/train.jpg" alt="foo bar" title="title" /></p>' +
    WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf589;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '![foo](<url>)';
  Expected := '<p><img src="url" alt="foo" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf59;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo  ', '-----']);
  Expected := '<h2>Foo</h2>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf590;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '![](/url)';
  Expected := '<p><img src="/url" alt="" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf591;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['![foo][bar]', '', '[bar]: /url']);
  Expected := '<p><img src="/url" alt="foo" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf592;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['![foo][bar]', '', '[BAR]: /url']);
  Expected := '<p><img src="/url" alt="foo" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf593;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['![foo][]', '', '[foo]: /url "title"']);
  Expected := '<p><img src="/url" alt="foo" title="title" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf594;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['![*foo* bar][]', '',
    '[*foo* bar]: /url "title"']);
  Expected := '<p><img src="/url" alt="foo bar" title="title" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf595;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['![Foo][]', '', '[foo]: /url "title"']);
  Expected := '<p><img src="/url" alt="Foo" title="title" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf596;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['![foo] ', '[]', '', '[foo]: /url "title"']);
  Expected := String.Join(WikiLB,
    ['<p><img src="/url" alt="foo" title="title" />', '[]</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf597;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['![foo]', '', '[foo]: /url "title"']);
  Expected := '<p><img src="/url" alt="foo" title="title" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf598;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['![*foo* bar]', '',
    '[*foo* bar]: /url "title"']);
  Expected := '<p><img src="/url" alt="foo bar" title="title" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf599;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['![[foo]]', '', '[[foo]]: /url "title"']);
  Expected := String.Join(WikiLB, ['<p>![[foo]]</p>',
    '<p>[[foo]]: /url &quot;title&quot;</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf6;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '>'#9#9'foo';
  Expected := String.Join(WikiLB, ['<blockquote>', '<pre><code>  foo',
    '</code></pre>', '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf60;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo\', '----']);
  Expected := '<h2>Foo\</h2>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf600;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['![Foo]', '', '[foo]: /url "title"']);
  Expected := '<p><img src="/url" alt="Foo" title="title" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf601;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['!\[foo]', '', '[foo]: /url "title"']);
  Expected := '<p>![foo]</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf602;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['\![foo]', '', '[foo]: /url "title"']);
  Expected := '<p>!<a href="/url" title="title">foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf603;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<http://foo.bar.baz>';
  Expected :=
    '<p><a href="http://foo.bar.baz">http://foo.bar.baz</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf604;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<http://foo.bar.baz/test?q=hello&id=22&boolean>';
  Expected :=
    '<p><a href="http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean">http://foo.bar.baz/test?q=hello&amp;id=22&amp;boolean</a></p>'
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf605;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<irc://foo.bar:2233/baz>';
  Expected :=
    '<p><a href="irc://foo.bar:2233/baz">irc://foo.bar:2233/baz</a></p>' +
    WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf606;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<MAILTO:FOO@BAR.BAZ>';
  Expected :=
    '<p><a href="MAILTO:FOO@BAR.BAZ">MAILTO:FOO@BAR.BAZ</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf607;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<a+b+c:d>';
  Expected := '<p><a href="a+b+c:d">a+b+c:d</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf608;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<made-up-scheme://foo,bar>';
  Expected :=
    '<p><a href="made-up-scheme://foo,bar">made-up-scheme://foo,bar</a></p>' +
    WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf609;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<http://../>';
  Expected := '<p><a href="http://../">http://../</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf61;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '`Foo' + WikiLB + '----' + WikiLB + '`' + WikiLB + '' + WikiLB +
    '<a title="a lot' + WikiLB + '---' + WikiLB + 'of dashes"/>';
  Expected := String.Join(WikiLB, ['<h2>`Foo</h2>', '<p>`</p>',
    '<h2>&lt;a title=&quot;a lot</h2>', '<p>of dashes&quot;/&gt;</p>'])
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf610;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<localhost:5001/foo>';
  Expected :=
    '<p><a href="localhost:5001/foo">localhost:5001/foo</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf611;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<http://foo.bar/baz bim>';
  // Expected := '<p>&lt;http://foo.bar/baz bim&gt;</p>' + WikiLB;
  // extended url autolink
  Expected := '<p>&lt;<a hreF="http://foo.bar/baz">http://foo.bar/baz</a> bim&gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf612;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<http://example.com/\[\>';
  Expected :=
    '<p><a href="http://example.com/%5C%5B%5C">http://example.com/\[\</a></p>' +
    WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf613;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<foo@bar.example.com>';
  Expected :=
    '<p><a href="mailto:foo@bar.example.com">foo@bar.example.com</a></p>' +
    WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf614;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<foo+special@Bar.baz-bar0.com>';
  Expected :=
    '<p><a href="mailto:foo+special@Bar.baz-bar0.com">foo+special@Bar.baz-bar0.com</a></p>'
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf615;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<foo\+@bar.example.com>';
  Expected := '<p>&lt;foo+@bar.example.com&gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf616;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<>';
  Expected := '<p>&lt;&gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf617;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '< http://foo.bar >';
  // Expected := '<p>&lt; http://foo.bar &gt;</p>' + WikiLB;
  // extended url autolink
  Expected := '<p>&lt; <a href="http://foo.bar">http://foo.bar</a> &gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf618;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<m:abc>';
  Expected := '<p>&lt;m:abc&gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf619;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<foo.bar.baz>';
  Expected := '<p>&lt;foo.bar.baz&gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf62;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> Foo', '---']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>Foo</p>',
    '</blockquote>', '<hr />']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf620;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'http://example.com';
  // Expected := '<p>http://example.com</p>' + WikiLB;
  // extended url autolink
  Expected := '<p><a href="http://example.com">http://example.com</a></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf621;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo@bar.example.com';
  Expected := '<p>foo@bar.example.com</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf629;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['http://commonmark.org', '',
    '(Visit https://encrypted.google.com/search?q=Markup+(business))']);
  Expected := String.Join(WikiLB,
    ['<p><a href="http://commonmark.org">http://commonmark.org</a></p>',
    '<p>(Visit <a href="https://encrypted.google.com/search?q=Markup+(business)">https://encrypted.google.com/search?q=Markup+(business)</a>)</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf629_2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['http://commonmark.org', '',
    '(Visit https://encrypted.google.com/search?q=Markup+(business))&key=value)']);
  Expected := String.Join(WikiLB,
    ['<p><a href="http://commonmark.org">http://commonmark.org</a></p>',
    '<p>(Visit <a href="https://encrypted.google.com/search?q=Markup+(business))&amp;key=value">https://encrypted.google.com/search?q=Markup+(business))&amp;key=value</a>)</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf63;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> foo', 'bar', '===']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>foo', 'bar', '===</p>',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf636;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<a><bab><c2c>';
  Expected := '<p><a><bab><c2c></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf637;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<a/><b2/>';
  Expected := '<p><a/><b2/></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf638;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<a  /><b2', 'data="foo" >']);
  Expected := String.Join(WikiLB, ['<p><a  /><b2', 'data="foo" ></p>'])
    + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf639;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<a foo="bar" bam = ''baz <em>"</em>''',
    '_boolean zoop:33=zoop:33 />']);
  Expected := String.Join(WikiLB, ['<p><a foo="bar" bam = ''baz <em>"</em>''',
    '_boolean zoop:33=zoop:33 /></p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf64;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- Foo', '---']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>Foo</li>', '</ul>', '<hr />']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf640;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'Foo <responsive-image src="foo.jpg" />';
  Expected := '<p>Foo <responsive-image src="foo.jpg" /></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf641;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<33> <__>';
  Expected := '<p>&lt;33&gt; &lt;__&gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf642;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<a h*#ref="hi">';
  Expected := '<p>&lt;a h*#ref=&quot;hi&quot;&gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf643;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<a href="hi''> <a href=hi''>';
  Expected := '<p>&lt;a href=&quot;hi''&gt; &lt;a href=hi''&gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf644;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['< a><', 'foo><bar/ >', '<foo bar=baz',
    'bim!bop />']);
  Expected := String.Join(WikiLB, ['<p>&lt; a&gt;&lt;', 'foo&gt;&lt;bar/ &gt;',
    '&lt;foo bar=baz', 'bim!bop /&gt;</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf645;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<a href=''bar''title=title>';
  Expected := '<p>&lt;a href=''bar''title=title&gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf646;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '</a></foo >';
  Expected := '<p></a></foo ></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf647;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '</a href="foo">';
  Expected := '<p>&lt;/a href=&quot;foo&quot;&gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf648;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['foo <!-- this is a',
    'comment - with hyphen -->']);
  Expected := String.Join(WikiLB, ['<p>foo <!-- this is a',
    'comment - with hyphen --></p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf649;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo <!-- not a comment -- two hyphens -->';
  Expected := '<p>foo &lt;!-- not a comment -- two hyphens --&gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf65;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', 'Bar', '---']);
  Expected := String.Join(WikiLB, ['<h2>Foo', 'Bar</h2>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf650;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['foo <!--> foo -->', '',
    'foo <!-- foo--->']);
  Expected := String.Join(WikiLB, ['<p>foo &lt;!--&gt; foo --&gt;</p>',
    '<p>foo &lt;!-- foo---&gt;</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf651;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo <?php echo $a; ?>';
  Expected := '<p>foo <?php echo $a; ?></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf652;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo <!ELEMENT br EMPTY>';
  Expected := '<p>foo <!ELEMENT br EMPTY></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf653;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo <![CDATA[>&<]]>';
  Expected := '<p>foo <![CDATA[>&<]]></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf654;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo <a href="&ouml;">';
  Expected := '<p>foo <a href="&ouml;"></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf655;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo <a href="\*">';
  Expected := '<p>foo <a href="\*"></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf656;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '<a href="\"">';
  Expected := '<p>&lt;a href=&quot;&quot;&quot;&gt;</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf657;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['<strong> <title> <style> <em>', '',
    '<blockquote>', '  <xmp> is disallowed.  <XMP> is also disallowed.',
    '</blockquote>']);
  Expected := String.Join(WikiLB, ['<p><strong> &lt;title> &lt;style> <em></p>',
    '<blockquote>', '  &lt;xmp> is disallowed.  &lt;XMP> is also disallowed.',
    '</blockquote>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf658;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo  ' + WikiLB + 'baz';
  Expected := '<p>foo<br />' + WikiLB + 'baz</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf659;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo\' + WikiLB + 'baz';
  Expected := '<p>foo<br />' + WikiLB + 'baz</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf66;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['---', 'Foo', '---', 'Bar', '---', 'Baz']);
  Expected := String.Join(WikiLB, ['<hr />', '<h2>Foo</h2>', '<h2>Bar</h2>',
    '<p>Baz</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf660;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo       ' + WikiLB + 'baz';
  Expected := '<p>foo<br />' + WikiLB + 'baz</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf661;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo  ' + WikiLB + '     bar';
  Expected := '<p>foo<br />' + WikiLB + 'bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf662;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo\' + WikiLB + '     bar';
  Expected := '<p>foo<br />' + WikiLB + 'bar</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf669;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo\';
  Expected := '<p>foo\</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf67;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := WikiLB + '====';
  Expected := '<p>====</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf670;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo  ';
  Expected := '<p>foo</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf671;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '### foo\';
  Expected := '<h3>foo\</h3>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf672;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '### foo  ';
  Expected := '<h3>foo</h3>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf673;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo' + WikiLB + 'baz';
  Expected := '<p>foo' + WikiLB + 'baz</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf674;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'foo ' + WikiLB + ' baz';
  Expected := '<p>foo' + WikiLB + 'baz</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf675;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'hello $.;''there';
  Expected := '<p>hello $.;''there</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf676;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'Foo χρῆν';
  Expected := '<p>Foo χρῆν</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf677;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := 'Multiple     spaces';
  Expected := '<p>Multiple     spaces</p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf68;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['---', '---']);
  Expected := String.Join(WikiLB, ['<hr />', '<hr />']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf69;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- foo', '-----']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo</li>', '</ul>', '<hr />']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf7;
begin
  raise ENotImplemented.Create('Need impliment.');
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf70;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['    foo', '---']);
  Expected := String.Join(WikiLB, ['<pre><code>foo', '</code></pre>', '<hr />']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf71;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> foo', '-----']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<p>foo</p>',
    '</blockquote>', '<hr />']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf72;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['\> foo', '------']);
  Expected := '<h2>&gt; foo</h2>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf73;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', '', 'bar', '---', 'baz']);
  Expected := String.Join(WikiLB, ['<p>Foo</p>', '<h2>bar</h2>', '<p>baz</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf74;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', 'bar', '', '---', '', 'baz']);
  Expected := String.Join(WikiLB, ['<p>Foo', 'bar</p>', '<hr />', '<p>baz</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf75;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', 'bar', '* * *', 'baz']);
  Expected := String.Join(WikiLB, ['<p>Foo', 'bar</p>', '<hr />', '<p>baz</p>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf76;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', 'bar', '\---', 'baz']);
  Expected := String.Join(WikiLB, ['<p>Foo', 'bar', '---', 'baz</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf77;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '    a simple' + WikiLB + '      indented code block';
  Expected := '<pre><code>a simple' + WikiLB + '  indented code block' + WikiLB
    + '</code></pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf77_2;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '    printf("Hello World!\n");';
  Expected := '<pre><code>printf(&quot;Hello World!\n&quot;);' + WikiLB +
    '</code></pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf77_3;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['    a simple', '      indented code block',
    '', '', 'foo']);
  Expected := String.Join(WikiLB, ['<pre><code>a simple',
    '  indented code block', '</code></pre>', '<p>foo</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf78;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['  - foo', '', '    bar']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>', '<p>foo</p>', '<p>bar</p>',
    '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf79;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['1.  foo', '', '    - bar']);
  Expected := String.Join(WikiLB, ['<ol>', '<li>', '<p>foo</p>', '<ul>',
    '<li>bar</li>', '</ul>', '</li>', '</ol>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf8;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['    foo', #9'bar']);
  Expected := String.Join(WikiLB, ['<pre><code>foo', 'bar', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf80;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['    <a/>', '    *hi*', '', '    - one']);
  Expected := String.Join(WikiLB, ['<pre><code>&lt;a/&gt;', '*hi*', '', '- one',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf81;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '    chunk1' + WikiLB + '' + WikiLB + '    chunk2' + WikiLB + '  '
    + WikiLB + ' ' + WikiLB + ' ' + WikiLB + '    chunk3';
  Expected := String.Join(WikiLB, ['<pre><code>chunk1', '', 'chunk2', '', '',
    '', 'chunk3', '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf82;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['    chunk1', '      ', '      chunk2']);
  Expected := String.Join(WikiLB, ['<pre><code>chunk1', '  ', '  chunk2',
    '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf83;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['Foo', '    bar']);
  Expected := String.Join(WikiLB, ['<p>Foo', 'bar</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf84;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['    foo', 'bar']);
  Expected := String.Join(WikiLB, ['<pre><code>foo', '</code></pre>',
    '<p>bar</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf85;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['# Heading', '    foo', 'Heading', '------',
    '    foo', '----']);
  Expected := String.Join(WikiLB, ['<h1>Heading</h1>', '<pre><code>foo',
    '</code></pre>', '<h2>Heading</h2>', '<pre><code>foo', '</code></pre>',
    '<hr />']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf86;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['        foo', '    bar']);
  Expected := String.Join(WikiLB, ['<pre><code>    foo', 'bar', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf87;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['', '    ', '    foo', '    ']);
  Expected := String.Join(WikiLB, ['<pre><code>foo', '</code></pre>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf88;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '    foo  ';
  Expected := String.Join(WikiLB, ['<pre><code>foo  ', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf89;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '```' + WikiLB + '<' + WikiLB + ' >' + WikiLB + '```';
  Expected := String.Join(WikiLB, ['<pre><code>&lt;', ' &gt;', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf9;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, [' - foo', '   - bar', #9' - baz']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo', '<ul>', '<li>bar', '<ul>',
    '<li>baz</li>', '</ul>', '</li>', '</ul>', '</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf90;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '~~~' + WikiLB + '<' + WikiLB + ' >' + WikiLB + '~~~';
  Expected := String.Join(WikiLB, ['<pre><code>&lt;', ' &gt;', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf91;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['``', 'foo', '``']);
  Expected := '<p><code>foo</code></p>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf92;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['```', 'aaa', '~~~', '```']);
  Expected := String.Join(WikiLB, ['<pre><code>aaa', '~~~', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf93;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['~~~', 'aaa', '```', '~~~']);
  Expected := String.Join(WikiLB, ['<pre><code>aaa', '```', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf94;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['````', 'aaa', '```', '``````']);
  Expected := String.Join(WikiLB, ['<pre><code>aaa', '```', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf95;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['~~~~', 'aaa', '~~~', '~~~~']);
  Expected := String.Join(WikiLB, ['<pre><code>aaa', '~~~', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf96;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := '```';
  Expected := '<pre><code></code></pre>' + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf97;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['`````', '', '```', 'aaa']);
  Expected := String.Join(WikiLB, ['<pre><code>', '```', 'aaa', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf98;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['> ```', '> aaa', '', 'bbb']);
  Expected := String.Join(WikiLB, ['<blockquote>', '<pre><code>aaa',
    '</code></pre>', '</blockquote>', '<p>bbb</p>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlGmf99;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['```', '', '  ', '```']);
  Expected := String.Join(WikiLB, ['<pre><code>', '  ', '</code></pre>']
    ) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

procedure TMarkdownConverterTest.TestWikiToHtmlNestedList;
var
  Expected: String;
  Sentence: String;
begin
  Sentence := String.Join(WikiLB, ['- foo', '  - bar', '  - bar2', '    - baz',
    '    - baz2', '  - bar3', '- foo2']);
  Expected := String.Join(WikiLB, ['<ul>', '<li>foo', '<ul>', '<li>bar</li>',
    '<li>bar2', '<ul>', '<li>baz</li>', '<li>baz2</li>', '</ul>', '</li>',
    '<li>bar3</li>', '</ul>', '</li>', '<li>foo2</li>', '</ul>']) + WikiLB;
  Assert.AreEqual(Expected, FConverter.WikiToHtml(Sentence));
end;

initialization

TDUnitX.RegisterTestFixture(TMarkdownConverterTest);

end.
