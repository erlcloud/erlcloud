defmodule Erlcloud.Mixfile do
use Mix.Project

    def project do
        [
            app: :erlcloud,
            version: "0.13.1",
            description: "Cloud Computing library for Erlang (Amazon EC2, S3, SQS, SimpleDB, Mechanical Turk, ELB)",
            deps: deps,
            language: :erlang
        ]
    end

    def application do
        [applications:
            [
                :kernel,
                :stdlib,
                :kernel,
                :crypto,
                :public_key,
                :ssl,
                :xmerl,
                :inets,
                :jsx,
                :lhttpc
            ],
        ]
    end

    def deps do
        [
            {:jsx, "2.8.0"},
            {:lhttpc, "1.4.0"},
        ]
    end
end
