// const { data } = require("jquery")

const { data } = require("jquery")

// const nav = document.querySelector('.site-header')
// fetch('./Navbar.html')
// .then(res=>res.text())
// .then(data=>{
//     nav.innerHTML=data
// })

const nav = document.querySelector('.site-header')
fetch('/Home/Navbar.html')
.then(res=>res.text())
.then(data=>{
    nav.innerHTML = data
})

