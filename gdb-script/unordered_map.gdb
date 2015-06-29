define puomap
    if $argc == 0
        help puomap
    else
        set $element_count = $arg0.m_element_count
        set $buckets = $arg0.m_buckets
        set $cur_bucket = $buckets
        set $cur_node = *$buckets
        set $index = 0
        while $index < $element_count
            if !$cur_node
                set $cur_bucket++
                while !(*$cur_bucket)
                    set $cur_bucket++
                end
                set $cur_node = *$cur_bucket
            end
            set $index++
            p (*$cur_node).m_v
            set $cur_node = (*$cur_node).m_next
            while !$cur_node
                set $cur_bucket++
                while !(*$cur_bucket)
                    set $cur_bucket++
                end
                set $cur_node = *$cur_bucket
            end
        end
    end
end
