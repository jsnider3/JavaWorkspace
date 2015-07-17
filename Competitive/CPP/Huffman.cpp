/* 
See HackerRank -> Data Structures -> Trees -> Huffman.
The structure of the node is

typedef struct node
{
    int freq;
    char data;
    node * left;
    node * right;
    
}node;

*/


void decode_huff(node * root, string s)
{
    node *decode = root;
    for (char c : s) {

        if (c == '0') {
            decode = decode->left;
        } else if (c == '1') {
            decode = decode->right;   
        }
        if (decode->data != '\0') {
            std::cout << decode->data;
            decode = root;
        }
    }
}
