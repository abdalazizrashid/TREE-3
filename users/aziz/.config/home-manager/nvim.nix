{pkgs, ...}: {
    enable = true;
    globals = {
      mapleader = " ";
      maplocalleader = " ";
    };
    options = {
      number = true;
      relativenumber = true;
      shiftwidth = 2;
      updatetime = 100;
      fileencoding = "utf-8";
    };
    filetype.extension.typ = "typst";

    keymaps = [
      {
        key = ";";
        action = ":";
      }
      {
        mode = "n";
        key = "<leader>m";
        options.silent = true;
        action = "<cmd>!make<CR>";
      }
      #      {
      #        key = "jk";
      #        action = "<Esc>";
      #      }
    ];
    plugins = {
#@       lsp = {
#@       enable = true;
#@        servers = {
#@          lua-ls.enable = true;
#@          pyright.enable = true;
#@          rnix-lsp.enable = true;
#@          html.enable = true;
#@          tsserver.enable = true;
#@          clangd.enable = true;
#@          elixirls.enable = true;
#@          rust-analyzer = {
#@            enable = true;
#@            installCargo = true;
#@            installRustc = true;
#@          };
#@        };
#@        keymaps = {
#@          diagnostic = {
#@            "<leader>j" = "goto_next";
#@            "<leader>k" = "goto_prev";
#@          };
#@          lspBuf = {
#@            K = "hover";
#@            gD = "references";
#@            gd = "definition";
#@            gi = "implementation";
#@            gy = "type_definition";
#@          };
#@        };
#@      };

      harpoon = {
        enable = true;
        keymaps = {
          addFile = "<leader>a";
          toggleQuickMenu = "<C-e>";
        };
      };
      telescope = {
        enable = true;
        defaults.file_ignore_patterns = [ "^.git/" ];
#        keymaps = {
#          "<leader>ff" = "find_file";
#          "<leader>fg" = "live_grep";
#          "<leader>fb" = "buffers";
#          "<leader>fh" = "help_tags";
#          "<C-p>" = {
#            action = "git_files";
#            desc = "Telescope Git Files";
#          };
#        };
      };
      luasnip = {
        enable = true;
        extraConfig = {
          enable_autosnippets = true;
          store_selection_keys = "<Tab>";
        };
      };
      friendly-snippets.enable = true;
    };
    extraPlugins = with pkgs.vimPlugins; [
      ctrlp
      editorconfig-vim
      nerdtree
      tabular
      vim-nix
      vim-markdown
      nvim-treesitter.withAllGrammars
      plenary-nvim
      cmp-nvim-lsp
      cmp-buffer
      cmp-path
      cmp-cmdline
      cmp_luasnip
      nvim-cmp
    ];
#    extraConfigLua = (builtins.readFile ~/.config/nvim/_init.lua);
}
